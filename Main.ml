open Util
open StarWark
open ParseCommon
open StarWarkParser

let source_of_string =
  ParseCommon.of_string
    StarWarkParser.source
    StarWarkLexer.token

let prog_of_string s =
  assemble (source_of_string s)

let imp = prog_of_string "

# program : imp
# author  : A. K. Dewdney (Scientific American 1984)
#
# An imp simply copies itself to the next address.

imp : imp + 1 <- @imp

"

let dwarf = prog_of_string "

# program : dwarf
# author  : A. K. Dewdney (Scientific American 1984)
#
# A dwarf copies a trap to regular intervals throughout memory.
# This one uses a stride length of 97.

init   : i  <- @stride
dwarf  : @i <- @trap
       : i  <- @i + @stride
       : goto dwarf
i      : DATA 0
trap   : DATA 0
stride : DATA 97

"

let spawn_imps = prog_of_string "

init   : i  <- @stride
loop   : @i <- @imp
       : spawn (@i - 1)
       : i  <- @i + @stride
       : goto loop
i      : DATA 0
imp    : imp + 1 <- @imp
stride : DATA 593

"

let spawn_dwarves = prog_of_string "

init    : i <- @stride
loop    : @i <- @(dwarf + 0)
        : @i <- @(dwarf + 1)
        : @i <- @(dwarf + 2)
        : @i <- @(dwarf + 3)
        : @i <- @(dwarf + 4)
        : @i <- @(dwarf + 5)
        : spawn (@i - 6)
        : i <- @i + @stride
        : if (@i < 10 * @stride) goto loop
imp     : imp + 1 <- @imp
i       : DATA 0
stride  : DATA 593
dwarf   : @di <- @trap
        : di  <- @di + @dstride
        : goto dwarf
di      : DATA 97
trap    : DATA 0
dstride : DATA 97

"

let () =
  let w = 125 in
  let h = 40 in
  let s =
    init_state (w * h)
      [ ("A", imp)
      ; ("B", dwarf)
      ; ("C", spawn_dwarves)
      ]
  in
  run (term_disp w 1) 100000 s

