# tauargus — ghm

The **ghm** module is the hypercube secondary-suppression method of
the τ-ARGUS engine (the legacy `GH(TabNo, AprioriPct, ModelSize,
ApplySingleton)` batch method). It is the documented protection route
for 5- and 6-dimensional tables and for linked table sets, where the
optimization-based suppression (csp) is limited to four dimensions.

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
  **secondarily unsafe**, plus the `proto001`-`proto003` diagnostic
  files; one table per call, explicit file names, no working
  directory;
- the core reads the result file back
  (`CGhmiter::SetSecondaryUnsafe`) and marks the cells.

There is no **control** file. The legacy control file (`TABELLE`)
existed to drive the external executable over many tables at once;
in the in-process flow the binding already loops over tables, so the
run-level parameters the control file carried (MINW/MAXW/NULW/ABSBET,
the quota, the `EXACT` 0/1/2 and weighting flags) are a `ghm_params`
struct the binding fills from the batch's GH command.

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
- `ghm_iter.c` — the iterative suppression algorithm (cube ratio,
  corner selection, protection-ratio degradation)
- `ghm_frozen.c` — frozen-cell handling (the `EXACT` 0/1/2
  behaviours)
- `ghm_output.c` — result file and proto diagnostics

`ghm.h` holds the public API and the shared structures. (Only the
entry point exists so far; the split lands with the first parsers.)

## Status

Port in progress (0.8.0 scope). The entry point `ghm_run`
(`src/ghm.h`) is validated up to the steering-file check and returns
`GHM_ERR_NOT_IMPLEMENTED` until the parsers and the algorithm land.
The port is verified against the Fortran original (built as a
development oracle) on a battery of tables, flat, hierarchical and
linked.

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

Tests (smoke test on the entry point):

```bash
cmake -S . -B build -DTAUARGUS_GHM_BUILD_TESTS=ON
cmake --build build -j
ctest --test-dir build --output-on-failure
```

## License

This fork is distributed under **EUPL-1.2**; see [LICENCE](LICENCE).
Original upstream EUPL 1.1 and copyright notices are retained. The later-version
permission in Article 5 of EUPL 1.1 is explained in [NOTICE.txt](NOTICE.txt).
