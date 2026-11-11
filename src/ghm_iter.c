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
 * required, every such hypercube is feasible; choose the feasible
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
    int ndim = t->ndim;
    if (ndim < 1 || ndim > 6) {
        snprintf(errbuf, errlen, "unsupported dimension %d", ndim);
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
    long total = stride[0] * (long)t->ncodes[0];

    gcell *grid = calloc((size_t)total, sizeof(gcell));
    if (!grid) {
        free(stride);
        snprintf(errbuf, errlen, "out of memory");
        return -1;
    }

    long *code = malloc((size_t)ndim * sizeof(long));   /* primary codes */
    long *nbrs = malloc((size_t)ndim * (size_t)total * sizeof(long)); /* per dim neighbour lists */
    long *stride_nbr = malloc((size_t)ndim * sizeof(long));
    long *cnt_nbr = malloc((size_t)ndim * sizeof(long));
    if (!code || !nbrs || !stride_nbr || !cnt_nbr) {
        free(stride); free(grid); free(code); free(nbrs);
        free(stride_nbr); free(cnt_nbr);
        snprintf(errbuf, errlen, "out of memory");
        return -1;
    }

    /* Place each cell at its grid index; primaries become status 1. */
    for (long i = 0; i < ncells; i++) {
        long idx = 0;
        for (int d = 0; d < ndim; d++)
            idx += cells[i].dims[d] * stride[d];
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
                                         &nbrs[(size_t)d * (size_t)total]);
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
            double cost = 0.0;
            for (long m = 0; m < masks; m++) {
                long gi = 0;
                for (int d = 0; d < ndim; d++) {
                    long j = (h / stride_nbr[d]) % cnt_nbr[d];
                    long c = (m & (1L << d)) ? nbrs[(size_t)d * (size_t)total + j]
                                             : code[d];
                    gi += c * stride[d];
                }
                if (gi == pidx)
                    continue;   /* the primary itself */
                if (grid[gi].status == 0) {   /* safe corner */
                    nsafe++;
                    double v = grid[gi].value;
                    cost += log(v > 0.0 ? v : 1.0);
                }
            }
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

        /* Suppress every safe corner of the chosen hypercube. */
        for (long m = 0; m < masks; m++) {
            long gi = 0;
            for (int d = 0; d < ndim; d++) {
                long j = (best_h / stride_nbr[d]) % cnt_nbr[d];
                long c = (m & (1L << d)) ? nbrs[(size_t)d * (size_t)total + j]
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

    free(stride);
    free(grid);
    free(code);
    free(nbrs);
    free(stride_nbr);
    free(cnt_nbr);
    return 0;
}
