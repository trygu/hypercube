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

/* Write a file with the given lines (each NUL-terminated, no newline
 * added). */
static void write_lines(const char *path, const char *const *lines, int n)
{
    FILE *fd = fopen(path, "w");
    if (!fd)
        return;
    for (int i = 0; i < n; i++)
        fputs(lines[i], fd), fputc('\n', fd);
    fclose(fd);
}

/* A flat 2D table, 3 codes per dimension. Simulates what
 * CGhmiter::ControlDataTable writes: for a non-hierarchical 3-code
 * dimension, MS = 2 (two levels), the group counts are "2 1" (the two
 * individual codes plus the total), and all three code indices are
 * listed in descending order. */
static void fixture_3x3(const char *path)
{
    const char *lines[] = {
        "'T2D_33'",
        "'(5(A8,1X)) '",
        "2",
        "2 2",
        "1 1",
        "0.0 0.0",
        "'Var 1'",
        "2 1",
        "00000002 00000001 00000000",
        "'Var 2'",
        "2 1",
        "00000002 00000001 00000000",
        "0.0 0.0",
        "1 1 1",
    };
    write_lines(path, lines, (int)(sizeof(lines) / sizeof(lines[0])));
}

/* A 2D table with 3 codes in the first dimension and 2 in the second,
 * and a non-default steering tail (QQUOTE DQUOTE = 5.0 0.5, GEWRAN
 * SCHAEZ IDBQUA = 2 1 0). The 2-code dimension has MS = 2, group
 * counts "1 1", and two listed codes. */
static void fixture_3x2(const char *path)
{
    const char *lines[] = {
        "'T2D_32'",
        "'(5(A8,1X)) '",
        "2",
        "2 2",
        "1 1",
        "0.0 0.0",
        "'Var 1'",
        "2 1",
        "00000002 00000001 00000000",
        "'Var 2'",
        "1 1",
        "00000001 00000000",
        "5.0 0.5",
        "2 1 0",
    };
    write_lines(path, lines, (int)(sizeof(lines) / sizeof(lines[0])));
}

/* A 1D table with 7 codes. The code list wraps at five per line, so the
 * seven code indices span two lines. Group counts "6 1", MS = 2. */
static void fixture_1d7(const char *path)
{
    const char *lines[] = {
        "'T1D_7'",
        "'(5(A8,1X)) '",
        "1",
        "2",
        "1",
        "0.0",
        "'Var 1'",
        "6 1",
        "00000006 00000005 00000004 00000003 00000002",
        "00000001 00000000",
        "0.0 0.0",
        "1 1 1",
    };
    write_lines(path, lines, (int)(sizeof(lines) / sizeof(lines[0])));
}

int main(void)
{
    ghm_table t;
    char err[512];
    int rc;

    /* fixture 1: 3 x 3 */
    fixture_3x3("ghm_steering_33.txt");
    err[0] = '\0';
    rc = ghm_parse_steering("ghm_steering_33.txt", &t, err, sizeof(err));
    CHECK(rc == 0, "3x3 parse succeeds");
    if (rc != 0)
        printf("  parse error: %s\n", err);
    if (rc == 0) {
        CHECK(t.ndim == 2, "3x3 ndim 2");
        CHECK(strcmp(t.table_name, "T2D_33") == 0, "3x3 table name");
        CHECK(t.ms[0] == 2 && t.ms[1] == 2, "3x3 ms [2,2]");
        CHECK(t.ncodes[0] == 3 && t.ncodes[1] == 3, "3x3 ncodes [3,3]");
        CHECK(t.groups[0][0] == 2 && t.groups[0][1] == 1, "3x3 groups0 [2,1]");
        CHECK(t.groups[1][0] == 2 && t.groups[1][1] == 1, "3x3 groups1 [2,1]");
        CHECK(t.codes[0][0] == 2 && t.codes[0][1] == 1 && t.codes[0][2] == 0,
              "3x3 codes0 [2,1,0]");
        CHECK(t.codes[1][0] == 2 && t.codes[1][1] == 1 && t.codes[1][2] == 0,
              "3x3 codes1 [2,1,0]");
        CHECK(t.qquote == 0.0 && t.dquote == 0.0, "3x3 qquote/dquote 0");
        CHECK(t.gewran == 1 && t.schaez == 1 && t.idbqua == 1,
              "3x3 gewran/schaez/idbqua 1 1 1");
        ghm_free_table(&t);
    }

    /* fixture 2: 3 x 2 with a non-default tail */
    fixture_3x2("ghm_steering_32.txt");
    err[0] = '\0';
    rc = ghm_parse_steering("ghm_steering_32.txt", &t, err, sizeof(err));
    CHECK(rc == 0, "3x2 parse succeeds");
    if (rc != 0)
        printf("  parse error: %s\n", err);
    if (rc == 0) {
        CHECK(t.ndim == 2, "3x2 ndim 2");
        CHECK(t.ncodes[0] == 3 && t.ncodes[1] == 2, "3x2 ncodes [3,2]");
        CHECK(t.groups[1][0] == 1 && t.groups[1][1] == 1,
              "3x2 groups1 [1,1]");
        CHECK(t.codes[1][0] == 1 && t.codes[1][1] == 0, "3x2 codes1 [1,0]");
        CHECK(t.qquote == 5.0 && t.dquote == 0.5, "3x2 tail qquote/dquote");
        CHECK(t.gewran == 2 && t.schaez == 1 && t.idbqua == 0,
              "3x2 tail gewran/schaez/idbqua 2 1 0");
        ghm_free_table(&t);
    }

    /* fixture 3: 1D with 7 codes, wrapped across two lines */
    fixture_1d7("ghm_steering_1d7.txt");
    err[0] = '\0';
    rc = ghm_parse_steering("ghm_steering_1d7.txt", &t, err, sizeof(err));
    CHECK(rc == 0, "1d7 parse succeeds");
    if (rc != 0)
        printf("  parse error: %s\n", err);
    if (rc == 0) {
        CHECK(t.ndim == 1, "1d7 ndim 1");
        CHECK(t.ms[0] == 2, "1d7 ms 2");
        CHECK(t.ncodes[0] == 7, "1d7 ncodes 7");
        CHECK(t.groups[0][0] == 6 && t.groups[0][1] == 1, "1d7 groups [6,1]");
        int ok = 1;
        for (int j = 0; j < 7; j++)
            if (t.codes[0][j] != 6 - j)
                ok = 0;
        CHECK(ok, "1d7 codes [6,5,4,3,2,1,0]");
        ghm_free_table(&t);
    }

    /* a missing file is a clean error */
    err[0] = '\0';
    rc = ghm_parse_steering("ghm_steering_missing.txt", &t, err, sizeof(err));
    CHECK(rc == -1, "missing file -> error");
    CHECK(t.ndim == 0, "no dims on error");
    CHECK(err[0] != '\0', "error message set");

    if (failures == 0)
        printf("ghm_steering_test: OK\n");
    return failures == 0 ? 0 : 1;
}
