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

    /* With a steering file present the entry point runs and reports
     * the port status (not implemented until the algorithm lands). */
    FILE *fd = fopen(steer, "w");
    CHECK(fd != NULL, "create steering file");
    if (fd) {
        fprintf(fd, "0\n1 1 0\n0.0 1000.0\n0.0\n0\n2\n1\n");
        fclose(fd);
    }
    CHECK(ghm_run(steer, cells, out, &p, &res) == GHM_ERR_NOT_IMPLEMENTED,
          "port-in-progress status reported");
    CHECK(res.code == GHM_ERR_NOT_IMPLEMENTED, "result code matches");
    CHECK(res.message[0] != '\0', "message filled");

    if (failures == 0)
        printf("ghm_smoke_test: OK\n");
    return failures == 0 ? 0 : 1;
}
