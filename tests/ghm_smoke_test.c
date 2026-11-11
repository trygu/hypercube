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
    const char *workdir = "ghm_smoke_workdir";
    ghm_result res;

    mkdir(workdir, 0755);

    /* No control file. */
    CHECK(ghm_run(workdir, &res) == GHM_ERR_NO_CONTROL,
          "missing control file -> GHM_ERR_NO_CONTROL");
    CHECK(res.code == GHM_ERR_NO_CONTROL, "result code matches");
    CHECK(res.message[0] != '\0', "message filled");

    /* Null arguments. */
    CHECK(ghm_run(NULL, &res) == GHM_ERR_BAD_ARGS, "null workdir rejected");
    CHECK(ghm_run(workdir, NULL) == GHM_ERR_BAD_ARGS, "null result rejected");

    /* With a control file present the entry point runs and reports
     * the port status (not implemented until the algorithm lands). */
    FILE *fd = fopen("ghm_smoke_workdir/ghm.control", "w");
    CHECK(fd != NULL, "create tabelle");
    if (fd) {
        fprintf(fd, "0\n1 1 0\n0.0 1000.0\n0.0\n0\n2\n1\n");
        fclose(fd);
    }
    CHECK(ghm_run(workdir, &res) == GHM_ERR_NOT_IMPLEMENTED,
          "port-in-progress status reported");
    CHECK(res.code == GHM_ERR_NOT_IMPLEMENTED, "result code matches");
    CHECK(res.message[0] != '\0', "message filled");

    if (failures == 0)
        printf("ghm_smoke_test: OK\n");
    return failures == 0 ? 0 : 1;
}
