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

#ifndef GHM_H
#define GHM_H

#ifdef __cplusplus
extern "C" {
#endif

/* Result codes returned by ghm_run (also in ghm_result.code). */
#define GHM_OK                    0
#define GHM_ERR_BAD_ARGS         -1
#define GHM_ERR_NO_TABELLE       -2  /* no tabelle control file in workdir */
#define GHM_ERR_PARSE_TABELLE    -3
#define GHM_ERR_PARSE_STEUER     -4
#define GHM_ERR_PARSE_EINGABE    -5
#define GHM_ERR_NOT_IMPLEMENTED -100 /* port in progress */

typedef struct {
    int code;          /* GHM_OK on success, a GHM_ERR_* otherwise */
    long n_secondary;  /* cells marked secondarily unsafe, all tables */
    char message[512]; /* human-readable detail, empty on GHM_OK */
} ghm_result;

/*
 * Run the GHMITER (hypercube) secondary-suppression algorithm in
 * process. Replaces the legacy external GHMITER executable.
 *
 * File contract with the tauargus core (unchanged from the legacy
 * flow): the core writes, per table, a steuer steering file and an
 * eingabe cell-record file plus a tabelle control file; this library
 * reads them from workdir, runs the algorithm, and writes, per
 * table, the result file whose per-cell lines carry a leading token
 * of 1129 where the cell is set secondarily unsafe (the core reads
 * that file back via CGhmiter::SetSecondaryUnsafe).
 *
 * On success returns 0 and fills *res with code GHM_OK.
 * On failure returns the GHM_ERR_* code and fills *res with it.
 */
int ghm_run(const char *workdir, ghm_result *res);

#ifdef __cplusplus
}
#endif

#endif /* GHM_H */
