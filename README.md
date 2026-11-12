# tauargus — ghm

The **ghm** module is the hypercube secondary-suppression method of
the τ-ARGUS engine (the legacy `GH(TabNo, AprioriPct, ModelSize,
ApplySingleton)` batch method). This port supports individual flat tables
with one to six dimensions. Linked tables, hierarchies, weighted tables,
singleton protection and quantitative protection intervals are not implemented.

It is an in-process C port of the GHMITER 4 program
(Gießing/Repsilber), originally Copyright IT Nordrhein Westfalen,
EUPL 1.1, published by sdcTools at
<https://github.com/sdcTools/hypercube>. The original Fortran is kept
under `reference/fortran/` as the porting reference and the test
oracle; it is not part of the shipped build.

## File contract

The legacy tool ran GHMITER as an external executable launched by the
Java front-end; the core no longer launches anything. The record
formats are unchanged, and they are the same ones the core's
`CGhmiter` helpers already implement:

- the core writes, per table, a **steering** file (code structure,
  hierarchy levels) and a **cells** file (one record per cell; value
  type 129 marks a primary suppression);
- `ghm_run(steer, cells, result, params)` reads them, runs the
  algorithm, and writes the **result** file whose per-cell lines
  carry a leading token of `1129` where the cell is set
  **secondarily unsafe**; one table per call, explicit file names, no working
  directory;
- the core reads the result file back
  (`CGhmiter::SetSecondaryUnsafe`) and marks the cells.

There is no **control** file. The legacy control file (`TABELLE`)
existed to drive the external executable over many tables at once;
in the in-process flow the binding already loops over tables, so the
run-level parameters the control file carried remain in `ghm_params` for
API compatibility. Only the binding defaults are accepted: `minw=1`,
`maxw=100`, `nulw=absbet=qquote=dquote=0`, `gewran=schaez=exact=1`,
`mode=0` and `print1=0` or `1`. Other controls fail explicitly with
`GHM_ERR_UNSUPPORTED`; their legacy behavior is not implemented.

**Naming.** The legacy executable worked in German: files
`TABELLE`/`STEUER`/`EINGABE`/`AUSGABE` and identifiers such as
`WERTART` (value type), `GEWRAN` (weighting mode), `QQUOTE` (cube
ratio). The C port uses English names and identifiers. Choosing
English file names changes no format; the Fortran oracle runs with
its own German names. A mapping table from the C identifiers to the
Fortran originals is kept with the port for traceability.

The algorithm is self-contained: no solver dependency (the legacy
program called no external LP solver either).

## Source layout

One file per responsibility, following the other engine submodules:

- `ghm.c` — entry point `ghm_run` (explicit file names, one table
  per call)
- `ghm_steering.c` — steering file parser (codes, hierarchy levels)
- `ghm_cells.c` — cells file parser (per-cell records)
- `ghm_iter.c` — flat hypercube selection and input/grid validation

`ghm.h` holds the public API and shared structures. Result records are
written by `ghm.c`; proto diagnostic files are not emitted.

## Supported scope and validation

For every primary, the algorithm selects a hypercube with all nonzero
corners, minimizing the number of additional suppressions, then logarithmic
cost. Zero corners are ineligible, including zeros already marked primary.
If no feasible cube exists, the call fails before writing a result. Existing
result files are not removed on failure: callers must check the return code.

The cells file must contain exactly one record per Cartesian grid position,
including totals and zero cells. Dimensions, coordinates, duplicate keys,
record counts, numeric values, flags and size arithmetic are validated before
indexing the grid. Values must be finite and nonnegative. The caller remains
responsible for consistent additive totals.

This implements the exact-reconstruction criterion, not the full legacy
GHMITER protection model. In particular, `fehl0`/`fehl1` protection levels
are carried through but not enforced. Default `minw`/`maxw` do not impose
bounds on the inferred value. Independently audit realized intervals against
the publication requirements. A successful run alone does not establish that
those requirements are met.

Regression tests cover selected flat Fortran comparisons, additive zero-cell
handling, one- to six-dimensional grids, malformed inputs and rejected
controls. They do not establish hierarchical, linked-table or quantitative
protection equivalence with the Fortran program.

## Build

Part of the superbuild in the top-level repo
(`tauargus-engine`, `engine/CMakeLists.txt`):

```bash
cmake -S engine -B engine/build \
      -DHighs_DIR=$(brew --prefix)/lib/cmake/Highs
cmake --build engine/build -j
```

Standalone:

```bash
cmake -S . -B build
cmake --build build -j
```

Tests (parsers, flat iteration and safety regressions):

```bash
cmake -S . -B build -DTAUARGUS_GHM_BUILD_TESTS=ON
cmake --build build -j
ctest --test-dir build --output-on-failure
```

## License

This fork is distributed under **EUPL-1.2**; see [LICENCE](LICENCE).
Original upstream EUPL 1.1 and copyright notices are retained. The later-version
permission in Article 5 of EUPL 1.1 is explained in [NOTICE.txt](NOTICE.txt).
