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
 * ghm_iter_test: drives ghm_run over the flat 2D ground-truth cases
 * and checks the result file, line by line, against the known-correct
 * answer captured from the gfortran oracle (tests/oracle/flat2d*).
 *
 * The fixtures here are embedded (copied byte-for-byte from the oracle
 * inputs) so the test is self-contained. The comparison is on the
 * contract the core actually reads back: the leading token per line
 * (1 = safe, 129 = primary, 1129 = secondary) and the cell's two key
 * indices, in the same order as the cells file. The float field widths
 * are implementation detail and not checked.
 *
 * Line order is the core WriteCell order: the last dimension varies
 * slowest, every dimension descending from its highest index to the
 * total (index 0). For a 3x3 flat table that is the fixed sequence
 * (2,2) (1,2) (0,2) (2,1) (1,1) (0,1) (2,0) (1,0) (0,0).
 */

#include <stdio.h>
#include <stdlib.h>
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

static void write_lines(const char *path, const char *const *lines, int n)
{
    FILE *fd = fopen(path, "w");
    if (!fd)
        return;
    for (int i = 0; i < n; i++)
        fputs(lines[i], fd), fputc('\n', fd);
    fclose(fd);
}

/* One expected result line: the leading token and the two key indices. */
typedef struct {
    int  wertart;
    long dim0, dim1;
} exp_line;

/* STEUER: flat 2D, 3 codes per dimension. Identical for all cases. */
static const char *steuer[] = {
    "'T2D'",
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

/* Cells: one primary (129) at (1,1). The oracle protects it with the
 * hypercube {0,1}x{0,1}, so (0,1),(1,0),(0,0) become secondary. */
static const char *eingabe_1[9] = {
    "1 18 90.0 1 1 90.0 90.0 '00000002' '00000002' '00000002' '00000002'",
    "1 12 60.0 1 1 60.0 60.0 '00000001' '00000002' '00000001' '00000002'",
    "1 6 30.0 1 1 30.0 30.0 '00000000' '00000002' '00000000' '00000002'",
    "1 16 80.0 1 1 80.0 80.0 '00000002' '00000001' '00000002' '00000001'",
    "129 10 50.0 1 1 50.0 50.0 '00000001' '00000001' '00000001' '00000001'",
    "1 4 20.0 1 1 20.0 20.0 '00000000' '00000001' '00000000' '00000001'",
    "1 14 70.0 1 1 70.0 70.0 '00000002' '00000000' '00000002' '00000000'",
    "1 8 40.0 1 1 40.0 40.0 '00000001' '00000000' '00000001' '00000000'",
    "1 2 10.0 1 1 10.0 10.0 '00000000' '00000000' '00000000' '00000000'",
};
static const exp_line expected_1[9] = {
    {1, 2, 2}, {1, 1, 2}, {1, 0, 2}, {1, 2, 1}, {129, 1, 1},
    {1129, 0, 1}, {1, 2, 0}, {1129, 1, 0}, {1129, 0, 0},
};

/* Cells: one primary (129) at (1,2). Four candidate hypercubes all
 * need three more suppressions; the oracle picks the minimum log-cost
 * set {0,1}x{0,2}: (0,2),(1,0),(0,0). */
static const char *eingabe_cost[9] = {
    "1 18 90.0 1 1 90.0 90.0 '00000002' '00000002' '00000002' '00000002'",
    "129 12 60.0 1 1 60.0 60.0 '00000001' '00000002' '00000001' '00000002'",
    "1 6 30.0 1 1 30.0 30.0 '00000000' '00000002' '00000000' '00000002'",
    "1 16 80.0 1 1 80.0 80.0 '00000002' '00000001' '00000002' '00000001'",
    "1 10 50.0 1 1 50.0 50.0 '00000001' '00000001' '00000001' '00000001'",
    "1 4 20.0 1 1 20.0 20.0 '00000000' '00000001' '00000000' '00000001'",
    "1 14 70.0 1 1 70.0 70.0 '00000002' '00000000' '00000002' '00000000'",
    "1 8 40.0 1 1 40.0 40.0 '00000001' '00000000' '00000001' '00000000'",
    "1 2 10.0 1 1 10.0 10.0 '00000000' '00000000' '00000000' '00000000'",
};
static const exp_line expected_cost[9] = {
    {1, 2, 2}, {129, 1, 2}, {1129, 0, 2}, {1, 2, 1}, {1, 1, 1},
    {1, 0, 1}, {1, 2, 0}, {1129, 1, 0}, {1129, 0, 0},
};

/* Cells: two primaries (129) at (2,2) and (1,1). The shared hypercube
 * {1,2}x{1,2} protects both with just (1,2) and (2,1) as secondary. */
static const char *eingabe_shared[9] = {
    "129 18 90.0 1 1 90.0 90.0 '00000002' '00000002' '00000002' '00000002'",
    "1 12 60.0 1 1 60.0 60.0 '00000001' '00000002' '00000001' '00000002'",
    "1 6 30.0 1 1 30.0 30.0 '00000000' '00000002' '00000000' '00000002'",
    "1 16 80.0 1 1 80.0 80.0 '00000002' '00000001' '00000002' '00000001'",
    "129 10 50.0 1 1 50.0 50.0 '00000001' '00000001' '00000001' '00000001'",
    "1 4 20.0 1 1 20.0 20.0 '00000000' '00000001' '00000000' '00000001'",
    "1 14 70.0 1 1 70.0 70.0 '00000002' '00000000' '00000002' '00000000'",
    "1 8 40.0 1 1 40.0 40.0 '00000001' '00000000' '00000001' '00000000'",
    "1 2 10.0 1 1 10.0 10.0 '00000000' '00000000' '00000000' '00000000'",
};
static const exp_line expected_shared[9] = {
    {129, 2, 2}, {1129, 1, 2}, {1, 0, 2}, {1129, 2, 1}, {129, 1, 1},
    {1, 0, 1}, {1, 2, 0}, {1, 1, 0}, {1, 0, 0},
};

/* Parse one result line: the leading token plus the two key indices.
 * The keys are the only single-quoted 8-digit fields in the line. */
static int parse_result_line(const char *line, int *wertart,
                             long *dim0, long *dim1)
{
    if (sscanf(line, "%d", wertart) != 1)
        return -1;
    const char *q = strchr(line, '\'');
    if (!q || sscanf(q + 1, "%ld", dim0) != 1)
        return -1;
    const char *q2 = strchr(q + 1, '\'');
    if (!q2 || !(q2 = strchr(q2 + 1, '\'')))
        return -1;
    if (sscanf(q2 + 1, "%ld", dim1) != 1)
        return -1;
    return 0;
}

static void run_case(const char *name, const char *const *eingabe,
                     const exp_line *expected, long exp_nsec,
                     const ghm_params *p)
{
    write_lines("ghm_iter_stuer.txt", steuer, (int)(sizeof(steuer) / sizeof(steuer[0])));
    write_lines("ghm_iter_eingabe.txt", eingabe, 9);

    ghm_result res;
    int rc = ghm_run("ghm_iter_stuer.txt", "ghm_iter_eingabe.txt",
                     "ghm_iter_ausgabe.txt", p, &res);
    CHECK(rc == GHM_OK, name);
    if (rc != GHM_OK) {
        printf("  %s: rc=%d (%s)\n", name, rc, res.message);
        return;
    }
    if (res.n_secondary != exp_nsec) {
        printf("FAIL: %s n_secondary=%ld want %ld\n", name,
               res.n_secondary, exp_nsec);
        failures++;
    }

    FILE *fd = fopen("ghm_iter_ausgabe.txt", "r");
    CHECK(fd != NULL, "result file created");
    if (!fd)
        return;

    char line[512];
    int li = 0;
    while (fgets(line, sizeof(line), fd)) {
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0')
            continue;
        int w;
        long d0, d1;
        if (parse_result_line(line, &w, &d0, &d1) != 0) {
            printf("FAIL: %s line %d does not parse\n", name, li + 1);
            failures++;
            li++;
            continue;
        }
        if (li >= 9) {
            printf("FAIL: %s has more than 9 result lines\n", name);
            failures++;
            break;
        }
        if (w != expected[li].wertart || d0 != expected[li].dim0 ||
            d1 != expected[li].dim1) {
            printf("FAIL: %s line %d got (%d,%ld,%ld) want (%d,%ld,%ld)\n",
                   name, li + 1, w, d0, d1,
                   expected[li].wertart, expected[li].dim0,
                   expected[li].dim1);
            failures++;
        }
        li++;
    }
    fclose(fd);
    if (li != 9) {
        printf("FAIL: %s wrote %d lines, expected 9\n", name, li);
        failures++;
    }
}

/* --- flat n-D cases (3D..6D) ------------------------------------------
 *
 * The flat 2D cases above are hand-built ground truth. For 3D..6D the
 * fixtures are generated the same way as the oracle run: a flat table
 * with 3 codes per dimension, one primary at the center cell (code 1 in
 * every dimension), and a distinct positive value per cell (value =
 * position + 1) so the log costs never tie. With that value layout the
 * minimum-cost hypercube pairs every dimension with its sibling code 2,
 * so the secondary cells are exactly the {1,2}^n corners (the center
 * primary plus 2^n - 1 secondaries); every other cell is safe.
 */
static long ipow(long base, int exp)
{
    long r = 1;
    for (int i = 0; i < exp; i++)
        r *= base;
    return r;
}

static void write_steer_file(const char *path, int ndim, int n_codes)
{
    FILE *fd = fopen(path, "w");
    if (!fd)
        return;
    fprintf(fd, "'T%dD'\n", ndim);
    fprintf(fd, "'(5(A8,1X)) '\n");
    fprintf(fd, "%d\n", ndim);
    for (int i = 0; i < ndim; i++)
        fprintf(fd, "%s2", i ? " " : "");
    fprintf(fd, "\n");
    for (int i = 0; i < ndim; i++)
        fprintf(fd, "%s1", i ? " " : "");
    fprintf(fd, "\n");
    for (int i = 0; i < ndim; i++)
        fprintf(fd, "%s0.0", i ? " " : "");
    fprintf(fd, "\n");
    for (int i = 0; i < ndim; i++) {
        fprintf(fd, "'Var %d'\n", i + 1);
        fprintf(fd, "%d 1\n", n_codes - 1);
        for (int j = n_codes - 1; j >= 0; j--)
            fprintf(fd, "%s%08d", j == n_codes - 1 ? "" : " ", j);
        fprintf(fd, "\n");
    }
    fprintf(fd, "0.0 0.0\n");
    fprintf(fd, "1 1 1\n");
    fclose(fd);
}

static void write_eingabe_file(const char *path, int ndim, int n_codes)
{
    FILE *fd = fopen(path, "w");
    if (!fd)
        return;
    long total = ipow(n_codes, ndim);
    for (long i = 0; i < total; i++) {
        long c[8];
        for (int d = 0; d < ndim; d++)
            c[d] = (n_codes - 1) - ((i / ipow(n_codes, d)) % n_codes);
        int is_prim = 1;
        for (int d = 0; d < ndim; d++)
            if (c[d] != 1)
                is_prim = 0;
        double v = (double)(i + 1);
        fprintf(fd, "%d 0 %.2f 1 1 %.2f %.2f",
                is_prim ? 129 : 1, v, v, v);
        for (int d = 0; d < ndim; d++)
            fprintf(fd, " '%08ld'", c[d]);
        for (int d = 0; d < ndim; d++)
            fprintf(fd, " '%08ld'", c[d]);
        fprintf(fd, "\n");
    }
    fclose(fd);
}

/* Parse a result line: the leading wertart plus up to maxk single-quoted
 * key values. Returns the number of keys parsed, or -1 on a bad line. */
static int parse_result_line_nd(const char *line, int *wertart,
                                long *keys, int maxk)
{
    if (sscanf(line, "%d", wertart) != 1)
        return -1;
    int n = 0;
    const char *p = line;
    while (n < maxk && (p = strchr(p, '\'')) != NULL) {
        const char *e = strchr(p + 1, '\'');
        if (!e)
            return -1;
        char *end;
        long v = strtol(p + 1, &end, 10);
        if (end == p + 1)
            return -1;
        keys[n++] = v;
        p = e + 1;
    }
    return n;
}

static void run_ndim_case(int ndim, int n_codes, const ghm_params *p)
{
    char name[32];
    snprintf(name, sizeof name, "flat%dd", ndim);

    write_steer_file("ghm_nd_stuer.txt", ndim, n_codes);
    write_eingabe_file("ghm_nd_eingabe.txt", ndim, n_codes);

    ghm_result res;
    int rc = ghm_run("ghm_nd_stuer.txt", "ghm_nd_eingabe.txt",
                     "ghm_nd_ausgabe.txt", p, &res);
    if (rc != GHM_OK) {
        printf("FAIL: %s: rc=%d (%s)\n", name, rc, res.message);
        failures++;
        return;
    }

    long total = ipow(n_codes, ndim);
    long exp_nsec = ipow(2, ndim) - 1;
    long exp_safe = total - ipow(2, ndim);
    if (res.n_secondary != exp_nsec) {
        printf("FAIL: %s n_secondary=%ld want %ld\n", name,
               res.n_secondary, exp_nsec);
        failures++;
    }

    FILE *fd = fopen("ghm_nd_ausgabe.txt", "r");
    if (!fd) {
        printf("FAIL: %s result file missing\n", name);
        failures++;
        return;
    }

    char line[1024];
    long li = 0, npri = 0, nsec = 0, nsafe = 0;
    while (fgets(line, sizeof line, fd)) {
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0')
            continue;
        int w;
        long keys[8];
        int nk = parse_result_line_nd(line, &w, keys, 8);
        if (nk != ndim) {
            printf("FAIL: %s line %ld parsed %d keys, want %d\n",
                   name, li + 1, nk, ndim);
            failures++;
            continue;
        }
        /* The result must be in the cells-file (WriteCell) order, which
         * is the mixed-radix position the keys encode. */
        long rank = 0;
        for (int d = 0; d < ndim; d++)
            rank += (long)(n_codes - 1 - keys[d]) * ipow(n_codes, d);
        if (rank != li) {
            printf("FAIL: %s line %ld: keys at rank %ld, order broken\n",
                   name, li + 1, rank);
            failures++;
        }
        /* Expected value type: the center cell is the primary; the
         * {1,2}^n corners are secondary; everything else is safe. */
        int all_one = 1, all_12 = 1;
        for (int d = 0; d < ndim; d++) {
            if (keys[d] != 1)
                all_one = 0;
            if (keys[d] != 1 && keys[d] != 2)
                all_12 = 0;
        }
        int exp = all_one ? 129 : (all_12 ? 1129 : 1);
        if (w != exp) {
            printf("FAIL: %s line %ld wertart=%d want %d\n", name, li + 1,
                   w, exp);
            failures++;
        }
        if (w == 129)
            npri++;
        else if (w == 1129)
            nsec++;
        else
            nsafe++;
        li++;
    }
    fclose(fd);

    if (li != total) {
        printf("FAIL: %s wrote %ld lines, want %ld\n", name, li, total);
        failures++;
    }
    if (npri != 1 || nsec != exp_nsec || nsafe != exp_safe) {
        printf("FAIL: %s counts pri=%ld sec=%ld safe=%ld want 1/%ld/%ld\n",
               name, npri, nsec, nsafe, exp_nsec, exp_safe);
        failures++;
    }
}

int main(void)
{
    /* Control-file (TABELLE) run parameters for the flat 2D cases. */
    ghm_params p;
    memset(&p, 0, sizeof(p));
    p.minw = 1.0;
    p.maxw = 100.0;
    p.nulw = 0.0;
    p.absbet = 0.0;
    p.qquote = 0.0;
    p.dquote = 0.0;
    p.gewran = 1;
    p.schaez = 1;
    p.mode = 0;
    p.print1 = 1;
    p.exact = 1;

    run_case("flat2d", eingabe_1, expected_1, 3, &p);
    run_case("flat2d_cost", eingabe_cost, expected_cost, 3, &p);
    run_case("flat2d_shared", eingabe_shared, expected_shared, 2, &p);

    /* n-D flat cases: distinct values -> the {1,2}^n hypercube. The
     * supported exact-only mode has no quantitative interval controls. */
    ghm_params pn;
    memcpy(&pn, &p, sizeof pn);
    run_ndim_case(3, 3, &pn);
    run_ndim_case(4, 3, &pn);
    run_ndim_case(5, 3, &pn);
    run_ndim_case(6, 3, &pn);

    if (failures == 0)
        printf("ghm_iter_test: OK\n");
    return failures == 0 ? 0 : 1;
}
