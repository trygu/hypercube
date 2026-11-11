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
#include <string.h>

#include "ghm.h"

static int failures = 0;

#define CHECK(cond, msg) \
    do { \
        if (!(cond)) { \
            printf("FAIL: %s\n", msg); \
            failures++; \
        } \
    } while (0)

int main(void)
{
    const char *path = "ghm_cells_test_cells.txt";
    ghm_cell *cells = NULL;
    long n = 0;
    char err[512];
    int rc;

    /* A 2D table: the first record is primary-unsafe (wertart 129),
     * the rest safe (wertart 1). The record layout matches
     * CGhmiter::WriteCell: seven numeric fields, then ndim quoted
     * dimension keys and ndim quoted code strings. */
    FILE *fd = fopen(path, "w");
    CHECK(fd != NULL, "create cells file");
    if (fd) {
        fprintf(fd, "  129    50   50.00 1   1  0.00 0.00 "
                    "'00000002' '00000002' '2' '2'\n");
        fprintf(fd, "    1     8   20.00 1   1  0.00 0.00 "
                    "'00000002' '00000001' '2' '1'\n");
        fprintf(fd, "    1    15   30.00 1   1  0.00 0.00 "
                    "'00000001' '00000002' '1' '2'\n");
        fprintf(fd, "    1    31  100.00 1   1  0.00 0.00 "
                    "'00000000' '00000000' '0' '0'\n");
        fclose(fd);
    }

    err[0] = '\0';
    rc = ghm_parse_cells(path, &cells, &n, err, sizeof(err));
    CHECK(rc == 0, "parse succeeds");
    if (rc != 0)
        printf("  parse error: %s\n", err);
    CHECK(n == 4, "four cells parsed");

    if (n == 4) {
        CHECK(cells[0].wertart == 129, "cell0 wertart 129 (primary unsafe)");
        CHECK(cells[0].freq == 50, "cell0 freq 50");
        CHECK(cells[0].resp == 50.0, "cell0 resp 50.0");
        CHECK(cells[0].gewi == 1 && cells[0].hoew == 1, "cell0 gewi/hoew 1");
        CHECK(cells[0].ndim == 2, "cell0 ndim 2");
        CHECK(cells[0].dims[0] == 2 && cells[0].dims[1] == 2,
              "cell0 dims [2,2]");
        CHECK(strcmp(cells[0].codes[0], "2") == 0 &&
              strcmp(cells[0].codes[1], "2") == 0, "cell0 codes [2,2]");

        CHECK(cells[1].wertart == 1, "cell1 wertart 1 (safe)");
        CHECK(cells[1].dims[0] == 2 && cells[1].dims[1] == 1,
              "cell1 dims [2,1]");
        CHECK(strcmp(cells[1].codes[1], "1") == 0, "cell1 code1 '1'");

        CHECK(cells[2].dims[0] == 1 && cells[2].dims[1] == 2,
              "cell2 dims [1,2]");
        CHECK(cells[3].resp == 100.0, "cell3 resp 100.0");
        CHECK(cells[3].dims[0] == 0 && cells[3].dims[1] == 0,
              "cell3 dims [0,0]");
    }

    ghm_free_cells(cells, n);

    /* A missing file is a clean error. */
    cells = NULL;
    n = 0;
    err[0] = '\0';
    rc = ghm_parse_cells("ghm_cells_missing.txt", &cells, &n, err,
                         sizeof(err));
    CHECK(rc == -1, "missing file -> error");
    CHECK(cells == NULL && n == 0, "no cells on error");
    CHECK(err[0] != '\0', "error message set");

    if (failures == 0)
        printf("ghm_cells_test: OK\n");
    return failures == 0 ? 0 : 1;
}
