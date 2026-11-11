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
Java front-end; the core no longer launches anything. The file
contract is unchanged, and it is the same one the core's
`CGhmiter` helpers already implement:

- the core writes a **tabelle** control file (dimension bound, table
  list, per-table file names) plus, per table, a **steuer** steering
  file (code structure, hierarchy levels) and an **eingabe** file
  (one record per cell; WERTART 129 marks a primary suppression);
- `ghm_run(workdir)` reads them, runs the algorithm, and writes, per
  table, the result file whose per-cell lines carry a leading token
  of `1129` where the cell is set **secondarily unsafe**, plus the
  `PROTO001`-`PROTO003` diagnostic files;
- the core reads the result file back
  (`CGhmiter::SetSecondaryUnsafe`) and marks the cells.

The algorithm is self-contained: no solver dependency (the legacy
program called no external LP solver either).

## Status

Port in progress (0.8.0 scope). The entry point `ghm_run`
(`src/ghm.h`) is validated up to the control-file check and returns
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

EUPL 1.1, same as the rest of the tauargus engine and the original
GHMITER source.
