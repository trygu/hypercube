/*
 * GHMITER (hypercube) secondary suppression for tau-ARGUS.
 *
 * C port of the GHMITER 4 program (Gießing/Repsilber), originally
 * Copyright IT Nordrhein Westfalen, published under EUPL 1.1 by
 * sdcTools (https://github.com/sdcTools/hypercube). The original
 * Fortran is kept in reference/fortran as the porting reference and
 * test oracle.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the European Union Public Licence
 * (EUPL) version 1.1, as published by the European Commission.
 *
 * You can find the text of the EUPL v1.1 on
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *
 * This software is distributed on an "AS IS" basis without
 * warranties or conditions of any kind, either express or implied.
 */

/*
 * ghm_iter.c: the hypercube algorithm for a flat (no substructure)
 * table, mode 0, no inferential-disclosure interval bound.
 *
 * Per 4.1 section 2.8.1: for each primary suppression, build every
 * hypercube with that cell as a corner point; when no interval bound is
 * required, hypercubes with all nonzero corners are feasible; choose the feasible
 * hypercube of minimum information loss (first the number of additional
 * suppressions, then the sum of the logarithmic costs of the cells that
 * would become secondarily unsafe); and suppress all of its corner
 * points. A primary that already lies in a hypercube whose other corners
 * are all suppressed is left alone.
 *
 * The hypercube sides are the 2-code groupings of one dimension that
 * contain the cell's code: for a flat dimension (one total code 0 plus
 * the individual codes 1..N-1) a code pairs with the total and with each
 * of its sibling individuals.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include <stdint.h>

#include "ghm_iter.h"

typedef struct {
    double value;   /* cell value (WERT) */
    int    status;  /* 0 safe, 1 primary, 2 secondary */
    int    file_idx;
} gcell;

/* The flat-dimension neighbours of code c in a dimension with ncodes
 * codes (0 = total, 1..N-1 = individuals): the total and the other
 * individuals. Fills nbrs[0..n) and returns n. */
static int flat_neighbours(int c, int ncodes, long *nbrs)
{
    int n = 0;
    if (c == 0) {
        for (int x = 1; x < ncodes; x++)
            nbrs[n++] = x;
    } else {
        nbrs[n++] = 0;
        for (int x = 1; x < ncodes; x++)
            if (x != c)
                nbrs[n++] = x;
    }
    return n;
}

int ghm_iter_run(const ghm_table *t, const ghm_cell *cells, long ncells,
                 const ghm_params *p, int *out_wertart, long *out_n_secondary,
                 char *errbuf, size_t errlen)
{
    (void)p;
    if (!t || !cells || !out_wertart || !out_n_secondary || ncells <= 0) {
        snprintf(errbuf, errlen, "missing or empty grid");
        return -1;
    }
    *out_n_secondary = 0;
    int ndim = t->ndim;
    if (ndim < 1 || ndim > 6) {
        snprintf(errbuf, errlen, "unsupported dimension %d", ndim);
        return -1;
    }

    /* Modified 2026-11-12: validate the entire grid before indexing it.
     * ncells and unique in-range keys establish Cartesian completeness. */
    long total = 1;
    int max_codes = 0;
    for (int d = 0; d < ndim; d++) {
        if (t->ms[d] != 2 || t->ncodes[d] < 1 ||
            t->groups[d][0] != t->ncodes[d] - 1 || t->groups[d][1] != 1) {
            snprintf(errbuf, errlen, "only flat dimensions are supported");
            return -1;
        }
        if (total > INT_MAX / t->ncodes[d]) {
            snprintf(errbuf, errlen, "table dimensions are too large");
            return -1;
        }
        total *= t->ncodes[d];
        if (t->ncodes[d] > max_codes) max_codes = t->ncodes[d];
    }
    if (ncells != total || (size_t)total > SIZE_MAX / sizeof(gcell) ||
        (size_t)max_codes > SIZE_MAX / ((size_t)ndim * sizeof(long))) {
        snprintf(errbuf, errlen, "cells must form a complete, size-safe grid");
        return -1;
    }

    /* Strides over the code-indexed grid; code values are 0..ncodes-1. */
    long *stride = malloc((size_t)ndim * sizeof(long));
    if (!stride) {
        snprintf(errbuf, errlen, "out of memory");
        return -1;
    }
    stride[ndim - 1] = 1;
    for (int d = ndim - 2; d >= 0; d--)
        stride[d] = stride[d + 1] * (long)t->ncodes[d + 1];

    gcell *grid = calloc((size_t)total, sizeof(gcell));
    if (!grid) {
        free(stride);
        snprintf(errbuf, errlen, "out of memory");
        return -1;
    }

    long *code = malloc((size_t)ndim * sizeof(long));   /* primary codes */
    long *nbrs = malloc((size_t)ndim * (size_t)max_codes * sizeof(long));
    long *stride_nbr = malloc((size_t)ndim * sizeof(long));
    long *cnt_nbr = malloc((size_t)ndim * sizeof(long));
    if (!code || !nbrs || !stride_nbr || !cnt_nbr) {
        free(stride); free(grid); free(code); free(nbrs);
        free(stride_nbr); free(cnt_nbr);
        snprintf(errbuf, errlen, "out of memory");
        return -1;
    }

    int result = -1;
    for (long i = 0; i < total; i++) grid[i].file_idx = -1;
    for (int d = 0; d < ndim; d++) {
        long *seen = &nbrs[(size_t)d * (size_t)max_codes];
        memset(seen, 0, (size_t)t->ncodes[d] * sizeof(long));
        for (int j = 0; j < t->ncodes[d]; j++) {
            long key = t->codes[d][j];
            if (key < 0 || key >= t->ncodes[d] || seen[key]) {
                snprintf(errbuf, errlen, "invalid or duplicate steering code");
                goto cleanup;
            }
            seen[key] = 1;
        }
    }

    /* Place each cell at its grid index; primaries become status 1. */
    for (long i = 0; i < ncells; i++) {
        if (cells[i].ndim != ndim || !cells[i].dims ||
            !isfinite(cells[i].resp) || cells[i].resp < 0 || !isfinite(cells[i].fehl0) ||
            !isfinite(cells[i].fehl1) || cells[i].freq < 0 ||
            (cells[i].wertart != 1 && cells[i].wertart != 129) ||
            cells[i].gewi != 1 || cells[i].hoew != 1) {
            snprintf(errbuf, errlen, "invalid cell dimensions, values or flags");
            goto cleanup;
        }
        long idx = 0;
        for (int d = 0; d < ndim; d++) {
            if (cells[i].dims[d] < 0 || cells[i].dims[d] >= t->ncodes[d]) {
                snprintf(errbuf, errlen, "cell coordinate out of range");
                goto cleanup;
            }
            idx += cells[i].dims[d] * stride[d];
        }
        if (grid[idx].file_idx != -1) {
            snprintf(errbuf, errlen, "duplicate cell coordinate");
            goto cleanup;
        }
        grid[idx].value = cells[i].resp;
        grid[idx].file_idx = (int)i;
        grid[idx].status = (cells[i].wertart == 129) ? 1 : 0;
    }

    long nsec = 0;
    long masks = 1L << ndim;   /* 2^ndim corners per hypercube */

    /* Process primaries in cell-file order (the core WriteCell order). */
    for (long i = 0; i < ncells; i++) {
        if (cells[i].wertart != 129)
            continue;
        long pidx = 0;
        for (int d = 0; d < ndim; d++) {
            code[d] = cells[i].dims[d];
            pidx += code[d] * stride[d];
        }

        /* Build the per-dimension neighbour lists for this primary. */
        for (int d = 0; d < ndim; d++)
            cnt_nbr[d] = flat_neighbours((int)code[d], t->ncodes[d],
                                         &nbrs[(size_t)d * (size_t)max_codes]);
        stride_nbr[ndim - 1] = 1;
        for (int d = ndim - 2; d >= 0; d--)
            stride_nbr[d] = stride_nbr[d + 1] * cnt_nbr[d + 1];
        long total_hc = stride_nbr[0] * cnt_nbr[0];

        /* Enumerate all hypercubes with this primary as a corner. */
        int already_protected = 0;
        int best_nsafe = 0, have_best = 0;
        double best_cost = 0.0;
        long best_h = 0;

        for (long h = 0; h < total_hc; h++) {
            int nsafe = 0;
            int feasible = 1;
            double cost = 0.0;
            for (long m = 0; m < masks; m++) {
                long gi = 0;
                for (int d = 0; d < ndim; d++) {
                    long j = (h / stride_nbr[d]) % cnt_nbr[d];
                    long c = (m & (1L << d)) ? nbrs[(size_t)d * (size_t)max_codes + j]
                                             : code[d];
                    gi += c * stride[d];
                }
                /* A zero corner cannot establish the nonzero-corner
                 * protection criterion, even if it is already suppressed. */
                if (grid[gi].value == 0.0) {
                    feasible = 0;
                    break;
                }
                if (gi == pidx)
                    continue;   /* the primary itself */
                if (grid[gi].status == 0) {   /* safe corner */
                    nsafe++;
                    double v = grid[gi].value;
                    cost += log(v > 0.0 ? v : 1.0);
                }
            }
            if (!feasible) continue;
            if (nsafe == 0)
                already_protected = 1;
            if (!have_best || nsafe < best_nsafe ||
                (nsafe == best_nsafe && cost < best_cost)) {
                have_best = 1;
                best_nsafe = nsafe;
                best_cost = cost;
                best_h = h;
            }
        }

        if (already_protected)
            continue;

        if (!have_best) {
            snprintf(errbuf, errlen, "no feasible nonzero hypercube for a primary cell");
            goto cleanup;
        }

        /* Suppress every safe corner of the chosen hypercube. */
        for (long m = 0; m < masks; m++) {
            long gi = 0;
            for (int d = 0; d < ndim; d++) {
                long j = (best_h / stride_nbr[d]) % cnt_nbr[d];
                long c = (m & (1L << d)) ? nbrs[(size_t)d * (size_t)max_codes + j]
                                         : code[d];
                gi += c * stride[d];
            }
            if (gi == pidx)
                continue;
            if (grid[gi].status == 0) {
                grid[gi].status = 2;
                nsec++;
            }
        }
    }

    /* Emit the final per-cell value type in cell-file order. */
    for (long i = 0; i < ncells; i++) {
        long idx = 0;
        for (int d = 0; d < ndim; d++)
            idx += cells[i].dims[d] * stride[d];
        int s = grid[idx].status;
        out_wertart[i] = (s == 2) ? 1129 : (s == 1) ? 129 : 1;
    }
    *out_n_secondary = nsec;
    result = 0;

cleanup:
    free(stride);
    free(grid);
    free(code);
    free(nbrs);
    free(stride_nbr);
    free(cnt_nbr);
    return result;
}
