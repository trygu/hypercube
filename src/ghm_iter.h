#ifndef GHM_ITER_H
#define GHM_ITER_H

#include <stddef.h>

#include "ghm.h"

/*
 * Run the hypercube secondary-suppression algorithm (the GHMITER core)
 * on the parsed table and cell records. out_wertart receives, in the same
 * order as cells, the final per-cell value type: 1 (safe), 129 (primary,
 * unchanged) or 1129 (secondarily unsafe, set by this run). out_n_secondary
 * is the number of cells set secondarily unsafe. Returns 0 on success,
 * -1 on error with errbuf set.
 *
 * This is the flat (no substructure) table path, mode 0, with no
 * inferential-disclosure interval bound, matching 4.1 section 2.8.1:
 * per primary, pick the minimum (additional-suppression count, then
 * logarithmic cost) hypercube and suppress all of its corners.
 */
int ghm_iter_run(const ghm_table *t, const ghm_cell *cells, long ncells,
                 const ghm_params *p, int *out_wertart, long *out_n_secondary,
                 char *errbuf, size_t errlen);

#endif /* GHM_ITER_H */
