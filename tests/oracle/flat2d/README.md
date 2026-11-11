# Oracle ground truth: flat 2D table, mode 0

A 3x3 table with one primary-unsafe cell (WertArt 129) at
dim1=1, dim2=1. The oracle's known-correct answer is three
secondary suppressions (WertArt 1129) at (0,1), (1,0) and (0,0),
the other corners of the hypercube {0,1} x {0,1} around the
primary cell.

## Files

- `TABELLE`: control file (INIT read order): memory 10000000,
  `0 1 1` (mode 0, print1 1, exact 1), MINW 1.0, MAXW 100.0,
  NULW 0.0, ABSBET 0, imxdim 2, tabanz 1, then the three file
  names.
- `STEUER`: steering file, the core `ControlDataTable` format.
  Each flat 3-code dim has MS 2: level 1 holds the two
  individual codes (indices 1, 2), level 2 the pseudo-total
  (index 0). Tail: `0.0 0.0` (QQUOTE DQUOTE), `1 1 1`
  (GEWRAN SCHAEZ IDBQUA).
- `EINGABE`: cells file, the core `WriteCell` format and order:
  the last dimension varies slowest, every dimension descends
  from its highest index to 0, i.e. the individual codes come
  before the total. Seven numerics (WertArt, Freq, Resp, 1, 1,
  MaxResp, MinResp) plus two 8-digit index keys per record.
- `expected_AUSGABE`: the oracle result. One line per cell in
  the same order as `EINGABE`; first token 1129 marks a
  secondary suppression. The engine reads it back one line per
  cell in `WriteCell` order, so the C result writer must emit
  every cell, not only the suppressed ones.

## Building and running the oracle

The pristine Fortran lives in `reference/fortran/`. gfortran
needs three harness patches that do not change the algorithm:

1. `OpenFile`: map `Stat='S'` to `STATUS='REPLACE'` instead of
   `'SCRATCH'` (gfortran rejects SCRATCH with a FILE=
   specifier, io 5002).
2. `OpenFile`: the non-scratch branch must keep
   `ACCESS='DIRECT',RECL=Icl` when `Acc='D'`; the original PC
   branch relied on the legacy compiler inferring direct access
   from RECL. Keep the OPEN lines within 72 columns.
3. `UPDA`: `INQUIRE (1, RECL=irecl)` returns 2147483647 for a
   formatted file under gfortran, so raise the `irecl.gt.800`
   guard to `irecl.gt.2147483647` (the record lengths were
   already validated when the cells were parsed).

Also strip the 0x1A bytes from the source first
(`LC_ALL=C tr -d '\032'`). Build:

    gfortran -I GHMiterSub -std=legacy -O2 -o ghmiter_oracle GHMITER4_2004.F

Run from a directory containing `TABELLE`, `STEUER` and
`EINGABE`; the program writes `AUSGABE`, `PROTO001`, `PROTO002`
and scratch files `FT09FILE`, `FT10FILE`, `FT12FILE` in place.

## Validating a C run

Compare the C result file to `expected_AUSGABE` by first token
and key pair per line (the float field widths differ between
implementations and are not part of the contract).
