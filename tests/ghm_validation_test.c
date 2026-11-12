/* Regression tests for flat GH protection and native input validation.
 * Copyright 2026 Trygve Tatsuya Falch. Licensed under EUPL-1.2.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ghm.h"

static int failures;
static const char *steer = "validation_steuer";
static const char *input = "validation_eingabe";
static const char *output = "validation_ausgabe";

#define CHECK(x, msg) do { if (!(x)) { \
    fprintf(stderr, "FAIL: %s\n", msg); failures++; } } while (0)

static ghm_params defaults(void)
{
    ghm_params p;
    memset(&p, 0, sizeof p);
    p.minw = 1; p.maxw = 100; p.gewran = 1; p.schaez = 1;
    p.print1 = 1; p.exact = 1;
    return p;
}

static void steering_options(const char *groups, const char *quota, const char *weights)
{
    FILE *f = fopen(steer, "w");
    fprintf(f, "'flat'\n'(5(A8,1X))'\n2\n2 2\n1 1\n0 0\n"
               "'A'\n%s\n2 1 0\n'B'\n2 1\n2 1 0\n%s\n%s\n", groups, quota, weights);
    fclose(f);
}

static void steering(const char *groups)
{
    steering_options(groups, "0 0", "1 1 1");
}

/* Additive table, with primary (1,1). Mode selects a malformed input. */
static void cells(int zero, int mode)
{
    const double positive[3][3] = {{21,9,12},{9,5,4},{12,4,8}};
    const double diagonal[3][3] = {{10,5,5},{5,5,0},{5,0,5}};
    FILE *f = fopen(input, "w");
    for (int b = 2; b >= 0; b--) {
        for (int a = 2; a >= 0; a--) {
            if (mode == 3 && a == 2 && b == 2) continue;
            int x = a, y = b;
            if (mode == 1 && a == 2 && b == 2) x = 99;
            if (mode == 4 && a == 2 && b == 2) x = 1;
            double v = zero ? diagonal[a][b] : positive[a][b];
            int w = a == 1 && b == 1 ? 129 : 1;
            if (mode == 2 && a == 2 && b == 2) {
                fprintf(f, "1 2 8 1 1 0 0 '2' 'label'\n");
            } else if (mode == 5 && a == 2 && b == 2) {
                fprintf(f, "1 2 nan 1 1 0 0 '2' '2' 'A' 'B'\n");
            } else if (mode == 6 && a == 2 && b == 2) {
                fprintf(f, "1 2 8 1 1 0 0 'bad' '2' 'A' 'B'\n");
            } else if (mode == 7 && a == 1 && b == 1) {
                fprintf(f, "999999999999999999999999 2 5 1 1 0 0 '1' '1' 'A' 'B'\n");
            } else if (mode == 8 && a == 1 && b == 1) {
                fprintf(f, "129 2 5 1 1 0 0 '1' '1' 'A' 'B\n");
            } else if (mode == 9 && a == 1 && b == 1) {
                fprintf(f, "129 2 5 1 1 0 0 '1' '1' 'A' 'B' garbage\n");
            } else if (mode == 10 && a == 1 && b == 1) {
                fprintf(f, "129 2 -5 1 1 0 0 '1' '1' 'A' 'B'\n");
            } else if (mode == 11 && a == 1 && b == 1) {
                fprintf(f, "129 2 5 1 9 0 0 '1' '1' 'A' 'B'\n");
            } else if (mode == 12 && a == 1 && b == 1) {
                fprintf(f, "129 2 0 1 1 0 0 '1' '1' 'A' 'B'\n");
            } else {
                fprintf(f, "%d %d %.1f 1 1 0 0 '%08d' '%08d' 'A' 'B'\n",
                        w, v == 0 ? 0 : 2, v, x, y);
            }
        }
    }
    fclose(f);
}

static int run(ghm_params *p, ghm_result *r)
{
    remove(output);
    return ghm_run(steer, input, output, p, r);
}

static void test_zero(void)
{
    ghm_params p = defaults();
    ghm_result r;
    steering("2 1"); cells(1, 0);
    CHECK(run(&p, &r) == GHM_OK, "zero table has a feasible nonzero cube");
    CHECK(r.n_secondary == 3, "zero table needs three secondary corners");
    FILE *f = fopen(output, "r");
    CHECK(f != NULL, "zero table output exists");
    if (!f) return;
    /* Same nonzero cube as the Fortran oracle: {0,1} x {0,1}. */
    const int expected[9] = {1,1,1,1,129,1129,1,1129,1129};
    char line[1024]; int i = 0;
    while (fgets(line, sizeof line, f)) {
        CHECK(i < 9 && atoi(line) == expected[i],
              "zero/frozen corners must not be used as secondary suppressions");
        i++;
    }
    CHECK(i == 9, "all cells retained in output order");
    fclose(f);
}

static void test_malformed(void)
{
    ghm_params p = defaults(); ghm_result r;
    for (int mode = 1; mode <= 12; mode++) {
        steering("2 1"); cells(0, mode);
        CHECK(run(&p, &r) != GHM_OK, "invalid cells must fail cleanly");
        CHECK(r.message[0] != '\0', "invalid cells report a reason");
        FILE *f = fopen(output, "r");
        CHECK(f == NULL, "invalid input must not create a success result");
        if (f) fclose(f);
    }
    steering("2147483647 1"); cells(0, 0);
    CHECK(run(&p, &r) != GHM_OK, "overflowing group counts are rejected");
    steering("1 2"); cells(0, 0);
    CHECK(run(&p, &r) != GHM_OK, "nonflat group structure is rejected");
    FILE *bad = fopen(steer, "w");
    fprintf(bad, "'bad total'\n'(5(A8,1X))'\n2\n2 2\n1 1\n0 0\n"
                 "'A'\n2 1\n2 0 1\n'B'\n2 1\n2 1 0\n0 0\n1 1 1\n");
    fclose(bad);
    CHECK(run(&p, &r) != GHM_OK, "total must be code 0, not an individual code");
    steering("2 1");
    FILE *f = fopen(input, "w"); fclose(f);
    CHECK(run(&p, &r) != GHM_OK, "empty grid is rejected");
}

static void test_settings(void)
{
    ghm_result r;
    steering("2 1"); cells(0, 0);
    for (int setting = 0; setting < 10; setting++) {
        ghm_params p = defaults();
        switch (setting) {
        case 0: p.mode = 4; break;
        case 1: p.qquote = 0.9; break;
        case 2: p.exact = 0; break;
        case 3: p.gewran = 2; break;
        case 4: p.minw = 0; break;
        case 5: p.maxw = 1000; break;
        case 6: p.nulw = 1; break;
        case 7: p.absbet = 1; break;
        case 8: p.dquote = 1; break;
        default: p.schaez = 2; break;
        }
        CHECK(run(&p, &r) == GHM_ERR_UNSUPPORTED, "unsupported setting must not be ignored");
        CHECK(r.message[0] != '\0', "unsupported setting reports a reason");
    }
    ghm_params p = defaults(); p.qquote = NAN;
    CHECK(run(&p, &r) == GHM_ERR_BAD_ARGS, "nonfinite parameters are rejected");
    p = defaults();
    steering_options("2 1", "0.5 0", "1 1 1");
    CHECK(run(&p, &r) == GHM_ERR_UNSUPPORTED, "steering quota is not ignored");
    steering_options("2 1", "0 0", "2 1 1");
    CHECK(run(&p, &r) == GHM_ERR_UNSUPPORTED, "steering weighting is not ignored");
    steering_options("2 1", "nan 0", "1 1 1");
    CHECK(run(&p, &r) == GHM_ERR_PARSE_STEERING, "nonfinite steering quota");
    steering_options("2 1", "0 0", "999999999999999999999999 1 1");
    CHECK(run(&p, &r) == GHM_ERR_PARSE_STEERING, "overflowing steering control");
}

int main(int argc, char **argv)
{
    test_zero();
    if (argc == 1 || strcmp(argv[1], "zero") != 0) {
        test_malformed();
        test_settings();
    }
    remove(steer); remove(input); remove(output);
    return failures ? 1 : 0;
}
