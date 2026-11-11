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

int ghm_run(const char *workdir, ghm_result *res)
{
    char path[4096];
    FILE *fd;

    if (!workdir || !res)
        return GHM_ERR_BAD_ARGS;

    memset(res, 0, sizeof(*res));

    snprintf(path, sizeof(path), "%s/ghm.control", workdir);
    fd = fopen(path, "r");
    if (!fd) {
        res->code = GHM_ERR_NO_CONTROL;
        snprintf(res->message, sizeof(res->message),
                 "no control file in %s", workdir);
        return res->code;
    }
    fclose(fd);

    /* Port in progress: the control/steering/cells parsers and the
     * algorithm follow (one source file each, see the README); the
     * entry point is validated up to the control-file check. */
    res->code = GHM_ERR_NOT_IMPLEMENTED;
    snprintf(res->message, sizeof(res->message),
             "GHMITER port in progress: algorithm not yet implemented");
    return res->code;
}
