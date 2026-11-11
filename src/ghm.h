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
#define GHM_ERR_BAD_ARGS          -1
#define GHM_ERR_NO_STEERING       -2  /* steering file missing */
#define GHM_ERR_PARSE_STEERING    -3
#define GHM_ERR_NO_CELLS          -4
#define GHM_ERR_PARSE_CELLS       -5
#define GHM_ERR_ALGORITHM         -6  /* internal algorithm error */
#define GHM_ERR_NOT_IMPLEMENTED  -100 /* port in progress */

/*
 * Run-level parameters that in the legacy flow lived in the control
 * file (TABELLE). The binding fills these from the batch's GH
 * suppress command. The per-dimension structure (codes, hierarchy
 * levels, boundary flags) comes from the steering file; the cell
 * values and the primary-suppression marker come from the cells file.
 * This is the initial set; it is finalized as the steering parser
 * lands (the COMMON /MWX/, /ABSBET/, /EXACT/ blocks of the Fortran).
 */
typedef struct {
    double minw;     /* lower bound of the safety interval */
    double maxw;     /* upper bound of the safety interval */
    double nulw;     /* value treated as null / below threshold */
    double absbet;   /* absolute tolerance */
    double qquote;   /* quota, relative part */
    double dquote;   /* quota, absolute part */
    int    gewran;   /* weighting mode */
    int    schaez;   /* threshold mode */
    int    mode;     /* 0, 2 or 4 (2 and 4 read weight parameters) */
    int    print1;   /* verbose diagnostics (0/1) */
    int    exact;    /* frozen-cell behaviour (0/1/2) */
} ghm_params;

typedef struct {
    int code;          /* GHM_OK on success, a GHM_ERR_* otherwise */
    long n_secondary;  /* cells marked secondarily unsafe */
    char message[512]; /* human-readable detail, empty on GHM_OK */
} ghm_result;

/*
 * Run the GHMITER (hypercube) secondary-suppression algorithm in
 * process. Replaces the legacy external GHMITER executable.
 *
 * The shape matches the other suppression modules (hitas, rounder):
 * the caller passes explicit file names and the library streams
 * them; there is no control file and no working directory, and the
 * caller owns the paths (in the binding, a scoped temp directory, as
 * in the MOD/OPT/RND flow). The tauargus core writes the steering
 * file (code structure, hierarchy levels) and the cells file (one
 * record per cell; value type 129 marks a primary suppression); this
 * library reads them, runs the algorithm, and writes the result file
 * whose per-cell lines carry a leading token of 1129 where the cell
 * is set secondarily unsafe (the core reads that file back via
 * CGhmiter::SetSecondaryUnsafe). The record formats are unchanged
 * from the legacy flow; the file names are English (see the README).
 * One table per call.
 *
 * On success returns 0 and fills *res with code GHM_OK.
 * On failure returns the GHM_ERR_* code and fills *res with it.
 */
int ghm_run(const char *steer_file, const char *cells_file,
            const char *result_file, const ghm_params *p, ghm_result *res);

#ifdef __cplusplus
}
#endif

#endif /* GHM_H */
