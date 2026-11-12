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

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

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
    const char *steer = "ghm_smoke_steer";
    const char *cells = "ghm_smoke_cells";
    const char *out = "ghm_smoke_out";
    ghm_params p;
    ghm_result res;
    FILE *fd;

    /* The files are written to the CWD with fixed names; drop leftovers
     * from a previous run so the missing-file checks are reliable. */
    remove(steer);
    remove(cells);
    remove(out);
    remove("ghm_smoke_steer2");

    memset(&p, 0, sizeof(p));
    p.minw = 0.0;
    p.maxw = 1000.0;
    p.exact = 0;

    /* Null arguments are rejected. */
    CHECK(ghm_run(NULL, cells, out, &p, &res) == GHM_ERR_BAD_ARGS,
          "null steering rejected");
    CHECK(ghm_run(steer, NULL, out, &p, &res) == GHM_ERR_BAD_ARGS,
          "null cells rejected");
    CHECK(ghm_run(steer, cells, NULL, &p, &res) == GHM_ERR_BAD_ARGS,
          "null result rejected");
    CHECK(ghm_run(steer, cells, out, NULL, &res) == GHM_ERR_BAD_ARGS,
          "null params rejected");
    CHECK(ghm_run(steer, cells, out, &p, NULL) == GHM_ERR_BAD_ARGS,
          "null result struct rejected");

    /* No steering file. */
    CHECK(ghm_run(steer, cells, out, &p, &res) == GHM_ERR_NO_STEERING,
          "missing steering file -> GHM_ERR_NO_STEERING");
    CHECK(res.code == GHM_ERR_NO_STEERING, "result code matches");
    CHECK(res.message[0] != '\0', "message filled");

    /* Steering present but the cells file is missing. */
    const char *steer2 = "ghm_smoke_steer2";
    /* A flat 2D table, 3x3, one primary (129) at (1,1): the oracle
     * fixture tests/oracle/flat2d, whose known answer is three
     * secondary suppressions (1129) at the other corners of the
     * hypercube around the primary. */
    static const char steer_text[] =
        "'T2D'\n"
        "'(5(A8,1X)) '\n"
        "2\n"
        "2 2\n"
        "1 1\n"
        "0.0 0.0\n"
        "'Var 1'\n"
        "2 1\n"
        "00000002 00000001 00000000\n"
        "'Var 2'\n"
        "2 1\n"
        "00000002 00000001 00000000\n"
        "0.0 0.0\n"
        "1 1 1\n";
    static const char cells_text[] =
        "    1 18 90.00 1   1  90.00 90.00 '00000002' '00000002' "
        "'00000002' '00000002'\n"
        "    1 12 60.00 1   1  60.00 60.00 '00000001' '00000002' "
        "'00000001' '00000002'\n"
        "    1  6 30.00 1   1  30.00 30.00 '00000000' '00000002' "
        "'00000000' '00000002'\n"
        "    1 16 80.00 1   1  80.00 80.00 '00000002' '00000001' "
        "'00000002' '00000001'\n"
        "  129 10 50.00 1   1  50.00 50.00 '00000001' '00000001' "
        "'00000001' '00000001'\n"
        "    1  4 20.00 1   1  20.00 20.00 '00000000' '00000001' "
        "'00000000' '00000001'\n"
        "    1 14 70.00 1   1  70.00 70.00 '00000002' '00000000' "
        "'00000002' '00000000'\n"
        "    1  8 40.00 1   1  40.00 40.00 '00000001' '00000000' "
        "'00000001' '00000000'\n"
        "    1  2 10.00 1   1  10.00 10.00 '00000000' '00000000' "
        "'00000000' '00000000'\n";
    fd = fopen(steer2, "w");
    CHECK(fd != NULL, "create steering file");
    if (fd) {
        fwrite(steer_text, 1, sizeof(steer_text) - 1, fd);
        fclose(fd);
    }
    CHECK(ghm_run(steer2, cells, out, &p, &res) == GHM_ERR_NO_CELLS,
          "missing cells file -> GHM_ERR_NO_CELLS");
    CHECK(res.code == GHM_ERR_NO_CELLS, "result code matches");
    CHECK(res.message[0] != '\0', "message filled");

    /* A real end-to-end run of the ported algorithm. */
    fd = fopen(cells, "w");
    CHECK(fd != NULL, "create cells file");
    if (fd) {
        fwrite(cells_text, 1, sizeof(cells_text) - 1, fd);
        fclose(fd);
    }
    p.minw = 1.0;
    p.maxw = 100.0;
    p.nulw = 0.0;
    p.exact = 1;
    CHECK(ghm_run(steer2, cells, out, &p, &res) == GHM_OK,
          "flat 2D run -> GHM_OK");
    CHECK(res.code == GHM_OK, "result code matches");
    CHECK(res.message[0] == '\0', "message empty on success");
    CHECK(res.n_secondary == 3, "three secondary suppressions");

    /* The result file marks exactly those cells 1129. */
    long n1129 = 0;
    FILE *ro = fopen(out, "r");
    CHECK(ro != NULL, "result file written");
    if (ro) {
        char line[1024];
        while (fgets(line, sizeof(line), ro)) {
            const char *t = line;
            while (*t == ' ' || *t == '\t')
                t++;
            if (strncmp(t, "1129", 4) == 0 && !isdigit((unsigned char)t[4]))
                n1129++;
        }
        fclose(ro);
    }
    CHECK(n1129 == 3, "result carries three 1129 lines");

    if (failures == 0)
        printf("ghm_smoke_test: OK\n");
    return failures == 0 ? 0 : 1;
}
