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

int ghm_run(const char *steer_file, const char *cells_file,
            const char *result_file, const ghm_params *p, ghm_result *res)
{
    FILE *fd;

    if (!steer_file || !cells_file || !result_file || !p || !res)
        return GHM_ERR_BAD_ARGS;

    memset(res, 0, sizeof(*res));

    /* Validate the steering file (the table structure) is readable. */
    fd = fopen(steer_file, "r");
    if (!fd) {
        res->code = GHM_ERR_NO_STEERING;
        snprintf(res->message, sizeof(res->message),
                 "cannot open steering file: %s", steer_file);
        return res->code;
    }
    fclose(fd);

    /* Port in progress: the steering/cells parsers and the algorithm
     * follow (one source file each, see the README); the entry point
     * is validated up to the steering-file check. */
    res->code = GHM_ERR_NOT_IMPLEMENTED;
    snprintf(res->message, sizeof(res->message),
             "GHMITER port in progress: algorithm not yet implemented");
    return res->code;
}
