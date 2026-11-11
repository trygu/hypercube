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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ghm.h"
#include "ghm_iter.h"

int ghm_run(const char *steer_file, const char *cells_file,
            const char *result_file, const ghm_params *p, ghm_result *res)
{
    char err[256];

    if (!steer_file || !cells_file || !result_file || !p || !res)
        return GHM_ERR_BAD_ARGS;

    memset(res, 0, sizeof(*res));
    err[0] = '\0';

    ghm_table t;
    if (ghm_parse_steering(steer_file, &t, err, sizeof(err))) {
        res->code = GHM_ERR_PARSE_STEERING;
        snprintf(res->message, sizeof(res->message), "steering: %s", err);
        return res->code;
    }

    ghm_cell *cells = NULL;
    long ncells = 0;
    if (ghm_parse_cells(cells_file, &cells, &ncells, err, sizeof(err))) {
        ghm_free_table(&t);
        res->code = GHM_ERR_PARSE_CELLS;
        snprintf(res->message, sizeof(res->message), "cells: %s", err);
        return res->code;
    }

    int *wertart = malloc((size_t)ncells * sizeof(int));
    long nsec = 0;
    int rc;
    if (!wertart ||
        (rc = ghm_iter_run(&t, cells, ncells, p, wertart, &nsec, err,
                           sizeof(err))) != 0) {
        ghm_free_table(&t);
        ghm_free_cells(cells, ncells);
        free(wertart);
        res->code = GHM_ERR_ALGORITHM;
        snprintf(res->message, sizeof(res->message), "algorithm: %s", err);
        return res->code;
    }

    /* Write the result, one line per cell in cell-file order. The core
     * reads back only the leading token (1129 = secondarily unsafe), so
     * the value type is authoritative; the rest is carried for
     * traceability, matching the legacy AUSGABE layout. */
    FILE *out = fopen(result_file, "w");
    if (!out) {
        ghm_free_table(&t);
        ghm_free_cells(cells, ncells);
        free(wertart);
        res->code = GHM_ERR_ALGORITHM;
        snprintf(res->message, sizeof(res->message),
                 "cannot open result file: %s", result_file);
        return res->code;
    }
    for (long i = 0; i < ncells; i++) {
        fprintf(out, "%8d%12ld%22.17f%22.16f%22.16f%22.17f%22.17f",
                wertart[i], cells[i].freq, cells[i].resp,
                (double)cells[i].gewi, (double)cells[i].hoew,
                cells[i].fehl0, cells[i].fehl1);
        for (int d = 0; d < t.ndim; d++)
            fprintf(out, " '%08ld'", cells[i].dims[d]);
        fprintf(out, "\n");
    }
    fclose(out);

    ghm_free_table(&t);
    ghm_free_cells(cells, ncells);
    free(wertart);

    res->code = GHM_OK;
    res->n_secondary = nsec;
    return GHM_OK;
}
