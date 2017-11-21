open Util

(** Programs *)

type expr =
  | Int   of int
  | Deref of expr
  (* bitwise *)
  | Lnot  of expr
  | Land  of expr * expr
  | Lor   of expr * expr
  | Lxor  of expr * expr
  | Lsl   of expr * expr
  | Lsr   of expr * expr
  (* arithmetic *)
  | Neg   of expr
  | Add   of expr * expr
  | Sub   of expr * expr
  | Mul   of expr * expr
  | Div   of expr * expr
  | Mod   of expr * expr
  (* boolean *)
  | Not   of expr
  | Eq    of expr * expr
  | Neq   of expr * expr
  | Lt    of expr * expr
  | Lte   of expr * expr
  | Gt    of expr * expr
  | Gte   of expr * expr
  | Conj  of expr * expr
  | Disj  of expr * expr
  (* conditional *)
  | Cond  of expr * expr * expr

let rec string_of_expr = function
  | Int   i        -> string_of_int i
  | Deref e1       -> string_of_uop "@"  e1
  | Lnot  e1       -> string_of_uop "~"  e1
  | Land  (e1, e2) -> string_of_bop "&"  e1 e2
  | Lor   (e1, e2) -> string_of_bop "|"  e1 e2
  | Lxor  (e1, e2) -> string_of_bop "^"  e1 e2
  | Lsl   (e1, e2) -> string_of_bop "<<" e1 e2
  | Lsr   (e1, e2) -> string_of_bop ">>" e1 e2
  | Neg   e1       -> string_of_uop "-"  e1
  | Add   (e1, e2) -> string_of_bop "+"  e1 e2
  | Sub   (e1, e2) -> string_of_bop "-"  e1 e2
  | Mul   (e1, e2) -> string_of_bop "*"  e1 e2
  | Div   (e1, e2) -> string_of_bop "/"  e1 e2
  | Mod   (e1, e2) -> string_of_bop "%"  e1 e2
  | Not   e1       -> string_of_uop "!"  e1
  | Eq    (e1, e2) -> string_of_bop "==" e1 e2
  | Neq   (e1, e2) -> string_of_bop "!=" e1 e2
  | Lt    (e1, e2) -> string_of_bop "<"  e1 e2
  | Lte   (e1, e2) -> string_of_bop "<=" e1 e2
  | Gt    (e1, e2) -> string_of_bop ">"  e1 e2
  | Gte   (e1, e2) -> string_of_bop ">=" e1 e2
  | Conj  (e1, e2) -> string_of_bop "&&" e1 e2
  | Disj  (e1, e2) -> string_of_bop "||" e1 e2
  | Cond (e1, e2, e3) ->
      Printf.sprintf "(%s ? %s : %s)"
        (string_of_expr e1)
        (string_of_expr e2)
        (string_of_expr e3)

and string_of_uop op e1 =
  Printf.sprintf "%s%s"
    op (string_of_expr e1)

and string_of_bop op e1 e2 =
  Printf.sprintf "(%s %s %s)"
    (string_of_expr e1) op (string_of_expr e2)

type cell =
  | Data of int
  | Asgn of expr * expr
  | Cjmp of expr * expr
  | Fork of expr

let string_of_cell = function
  | Data i ->
      Printf.sprintf "Data %d" i
  | Asgn (e1, e2) ->
      Printf.sprintf "%s <-- %s"
        (string_of_expr e1)
        (string_of_expr e2)
  | Cjmp (e1, e2) ->
      Printf.sprintf "if %s goto %s"
        (string_of_expr e1)
        (string_of_expr e2)
  | Fork e1 ->
      Printf.sprintf "fork %s"
        (string_of_expr e1)

type prog = cell list

let string_of_prog p =
  p |> List.map string_of_cell
    |> String.concat "\n"

(** States *)

(* new type to avoid confusion with addresses, negative means blank *)
type color =
  Color of int

let int_of_color = function
  | Color i -> i

type mem =
  (color * cell) array

let string_of_mem mem =
  let aux i (color, cell) =
    let Color c = color in
    Printf.sprintf "%05d | %02d : %s"
      i c (string_of_cell cell)
  in
  mem |> Array.mapi aux
      |> Array.to_list
      |> String.concat "\n"

type proc =
  { pc : int }

let string_of_proc p =
  Printf.sprintf "{pc = %d}"
    p.pc

type player =
  { name  : string
  ; color : color
  ; procs : proc list
  }

let string_of_player p =
  let pcs =
    p.procs
      |> List.map string_of_proc
      |> String.concat " "
  in
  Printf.sprintf "%-5s | %02d : %s"
    (String.sub (p.name ^ "     ") 0 5)
    (int_of_color p.color)
    pcs

type state =
  mem * (player list)

let string_of_queue q =
  q |> List.map string_of_player
    |> String.concat "\n"

let string_of_state (mem, queue) =
  String.concat "\n"
    [ "ADDR  | CO : CELL"
    ; "-----------------------------------------------"
    ; string_of_mem mem
    ; ""
    ; "PLAYER QUEUES"
    ; "-----------------------------------------------"
    ; string_of_queue queue
    ]

(** Interpreter *)

let init_mem n =
  Array.init n (fun _ ->
    (Color (-1), Data 0))

let load mem addr =
  mem |> Array.length
      |> posmod addr
      |> Array.get mem

let store mem addr cc =
  mem |> Array.length
      |> posmod addr
      |> flip (Array.set mem) cc

let install_prog mem color start prog =
  let aux i c =
    store mem (start + i) (color, c) in
  List.iteri aux prog

(* longer  programs loaded earlier  *)
(* shorter programs execute earlier *)
let init_state n pps =
  let cmp_pp (_, p1) (_, p2) =
    List.length p2 - List.length p1 in
  let mem = init_mem n in
  let npp = List.length pps in
  let aux i (name, prog) =
    let color = Color i in
    let start = i * (n / npp) in
    install_prog mem color start prog;
    { name  = name
    ; color = color
    ; procs = [{pc = start}]
    }
  in
  pps |> List.sort cmp_pp
      |> List.mapi aux
      |> List.rev
      |> pair mem

(* cleaner than monads in OCaml *)
exception Bogus of string

let bogus msg =
  raise (Bogus msg)

let step_p mem color proc =
  let data = function
    | Data i -> i
    | _ -> bogus "inspect non data"
  in
  let read c1 =
    let (co, cell) =
      load mem (proc.pc + data c1) in
    if co = color
    then cell
    else bogus "read wrong color"
  in
  let _div a b =
    if b = 0
    then bogus "division by zero"
    else a / b
  in
  let _mod a b =
    if b = 0
    then bogus "modulo by zero"
    else a mod b
  in
  let rec eval_e = function
    | Int i   -> Data i
    | Deref a -> read (eval_e a)
    (* bitwise *)
    | Lnot a      -> evali_uop (lnot) a
    | Land (a, b) -> evali_bop (land) a b
    | Lor  (a, b) -> evali_bop (lor)  a b
    | Lxor (a, b) -> evali_bop (lxor) a b
    | Lsl  (a, b) -> evali_bop (lsl)  a b
    | Lsr  (a, b) -> evali_bop (lsr)  a b
    (* arithmetic *)
    | Neg  a      -> evali_uop (~-)  a
    | Add  (a, b) -> evali_bop (+)   a b
    | Sub  (a, b) -> evali_bop (-)   a b
    | Mul  (a, b) -> evali_bop ( * ) a b
    | Div  (a, b) -> evali_bop _div  a b
    | Mod  (a, b) -> evali_bop _mod  a b
    (* boolean *)
    | Not  a      -> evali_uop (liftb_uop not) a
    | Eq   (a, b) -> Data (iob (eval_e a =  eval_e b))
    | Neq  (a, b) -> Data (iob (eval_e a <> eval_e b))
    | Lt   (a, b) -> evali_bop (liftb_bop (<))  a b
    | Lte  (a, b) -> evali_bop (liftb_bop (<=)) a b
    | Gt   (a, b) -> evali_bop (liftb_bop (>))  a b
    | Gte  (a, b) -> evali_bop (liftb_bop (>=)) a b
    | Conj (a, b) ->
        if boi (data (eval_e a))
        then Data (iob (boi (data (eval_e b))))
        else Data (iob false)
    | Disj (a, b) ->
        if boi (data (eval_e a))
        then Data (iob true)
        else Data (iob (boi (data (eval_e b))))
    (* conditional *)
    | Cond (a, b, c) ->
        if boi (data (eval_e a))
        then eval_e b
        else eval_e c

  and evali_uop op a =
    Data (op (data (eval_e a)))

  and evali_bop op a b =
    Data (op (data (eval_e a))
             (data (eval_e b)))
  in
  let write c1 c2 =
    store mem
      (proc.pc + data c1)
      (color, c2)
  in
  match read (Data 0) with
  | Data _ ->
      bogus "execute data"
  | Asgn (e1, e2) ->
      write (eval_e e1) (eval_e e2);
      [{pc = proc.pc + 1}]
  | Cjmp (e1, e2) ->
      if boi (data (eval_e e1))
      then [{pc = proc.pc + data (eval_e e2)}]
      else [{pc = proc.pc + 1}]
  | Fork e1 ->
      [ {pc = proc.pc + 1}
      ; {pc = proc.pc + data (eval_e e1)}
      ]

let step (mem, players) =
  match players with
  | [] -> (* all players' procs crashed *)
      (mem, [])
  | pl :: pls ->
      begin match pl.procs with
      | [] -> (* all pl's procs crashed *)
          (mem, pls)
      | p :: ps ->
          try
            let ps' = step_p mem pl.color p in
            (mem, pls @ [{pl with procs = ps @ ps'}])
          with Bogus msg -> (* p crashed *)
            (mem, pls @ [{pl with procs = ps}])
      end

let draw (mem, players) =
  match players with
  | [] -> true
  | _  -> false

let winner (mem, players) =
  match players with
  | [] -> None
  | pl :: [] ->
      begin match pl.procs with
      | [] -> None
      | _  -> Some pl.name
      end
  | _ -> None

type res =
  | Draw
  | Win of string

let string_of_res = function
  | Draw  -> "Draw"
  | Win s -> "Winner: " ^ s

let rec run ?winstop:(winstop = true) disp tm st =
  match winner st, winstop with
  | Some nm, true ->
      Win nm
  | _, _ ->
      if tm <= 0 || draw st then
        Draw
      else begin
        disp st;
        run disp (tm - 1) (step st)
      end

let clear () =
  print_string "\027[2J"

let nap dur =
  dur |> float_of_int
      |> flip (/.) 1000.0
      |> Unix.select [] [] []
      |> ignore

let debug_disp st =
  clear ();
  st |> string_of_state
     |> print_endline;
  ignore (read_line ())

let test_disp dur st =
  clear ();
  st |> string_of_state
     |> print_endline;
  nap dur

(* h/t https://github.com/Chris00/ANSITerminal
 * and https://en.wikipedia.org/wiki/ANSI_escape_code
 *)
let disp_cell (color, cell) =
  let s =
    (* TODO support > 5 players *)
    match int_of_color color, cell with
    | 0, Data _ -> "\027[1;31m*\027[0m"  (* red *)
    | 0, _      -> "\027[31m+\027[0m"
    | 1, Data _ -> "\027[1;32m*\027[0m"  (* green *)
    | 1, _      -> "\027[32m+\027[0m"
    | 2, Data _ -> "\027[1;34m*\027[0m"  (* blue *)
    | 2, _      -> "\027[34m+\027[0m"
    | 3, Data _ -> "\027[1;36m*\027[0m"  (* cyan *)
    | 3, _      -> "\027[36m+\027[0m"
    | 4, Data _ -> "\027[1;37m*\027[0m"  (* white *)
    | 4, _      -> "\027[37m+\027[0m"
    | _         -> "."
  in
  print_string s

let __dt_init = ref false
let disp_term w dur (mem, players) =
  if not !__dt_init then begin
    Printf.printf "\027[?25l"; (* disable cursor *)
    clear ();
    __dt_init := true
  end;
  let aux i c =
    if i mod w = 0 then begin
      Printf.printf "\027[E" (* next line *)
    end;
    disp_cell c
  in
  Printf.printf "\027[H"; (* top left *)
  Array.iteri aux mem;
  flush stdout;
  nap dur

let imp =
  [ Asgn (Int 1, Deref (Int 0)) ]

let dwarf n =
  [ Cjmp (Int 1, Int 3)
  ; Data n
  ; Data 0
  ; Asgn (Deref (Int (-2)), Deref (Int (-1)))
  ; Asgn (Int (-3), Add (Deref (Int (-3)), Int n))
  ; Cjmp (Int 1, Int (-5))
  ]

let () =
  at_exit (fun () ->
    (* enable cursor *)
    Printf.printf "\027[?25h");
  let w = 64 in
  let h = 32 in
  let s =
    init_state (w * h)
      [ ("A", imp)
      ; ("B", dwarf 593)
      ; ("C", dwarf 1171)
      ; ("D", dwarf 7)
      ]
  in
  s |> run (disp_term w 5) 10000
    |> string_of_res
    >> (fun _ -> print_newline ())
    |> print_endline
