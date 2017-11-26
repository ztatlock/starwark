open Util

module Q = Queue.Queue

(** Programs *)

type unop =
  | Deref | Lnot | Neg | Pos | Not

type binop =
  | Land | Lor | Lxor | Lsl  | Lsr
  | Add  | Sub | Mul  | Div  | Mod
  | Lt   | Le  | Gt   | Ge
  | Eq   | Neq | Conj | Disj

type expr =
  | Int   of int
  | Unop  of unop  * expr
  | Binop of binop * expr * expr
  | Cond  of expr  * expr * expr
  (* only for source programs *)
  | Lbl   of string

let unop op e =
  Unop (op, e)

let binop op e1 e2 =
  Binop (op, e1, e2)

let string_of_unop = function
  | Deref -> "@"
  | Lnot  -> "~"
  | Neg   -> "-"
  | Pos   -> "+"
  | Not   -> "!"

let string_of_binop = function
  | Land -> "&"
  | Lor  -> "|"
  | Lxor -> "^"
  | Lsl  -> "<<"
  | Lsr  -> ">>"
  | Add  -> "+"
  | Sub  -> "-"
  | Mul  -> "*"
  | Div  -> "/"
  | Mod  -> "%"
  | Lt   -> "<"
  | Le   -> "<="
  | Gt   -> ">"
  | Ge   -> ">="
  | Eq   -> "=="
  | Neq  -> "!="
  | Conj -> "&&"
  | Disj -> "||"

let rec string_of_expr = function
  | Int i ->
      string_of_int i
  | Unop (op, e1) ->
      Printf.sprintf "%s%s"
        (string_of_unop op)
        (string_of_expr e1)
  | Binop (op, e1, e2) ->
      Printf.sprintf "(%s %s %s)"
        (string_of_expr  e1)
        (string_of_binop op)
        (string_of_expr  e2)
  | Cond  (e1, e2, e3) ->
      Printf.sprintf "(%s ? %s : %s)"
        (string_of_expr e1)
        (string_of_expr e2)
        (string_of_expr e3)
  | Lbl l ->
      l

type cell =
  | Data of int
  | Asgn of expr * expr
  | Cjmp of expr * expr
  | Spwn of expr

let string_of_cell = function
  | Data i ->
      Printf.sprintf "DATA %d" i
  | Asgn (e1, e2) ->
      Printf.sprintf "%s <- %s"
        (string_of_expr e1)
        (string_of_expr e2)
  | Cjmp (e1, e2) ->
      begin match e1 with
      | Int 1 ->
          Printf.sprintf "goto %s"
            (string_of_expr e2)
      | _ ->
          Printf.sprintf "if %s goto %s"
            (string_of_expr e1)
            (string_of_expr e2)
      end
  | Spwn e1 ->
      Printf.sprintf "spawn %s"
        (string_of_expr e1)

type prog = cell list

let string_of_prog p =
  p |> List.mapi (fun i cell ->
        Printf.sprintf "%3d : %s"
          i (string_of_cell cell))
    |> String.concat "\n"

type source = (string * cell) list

let string_of_source src =
  src |> List.map (fun (lbl, cell) ->
          Printf.sprintf "%8s : %s"
            lbl (string_of_cell cell))
      |> String.concat "\n"

exception Label of string

let assemble src =
  let labtab =
    src |> List.map fst
        |> List.mapi (flip pair)
  in
  let labref pos lab =
    try List.assoc lab labtab - pos
    with Not_found -> raise (Label lab)
  in
  let rec delab_e pos = function
    | Int i ->
        Int i
    | Unop (op, e1) ->
        Unop (op, delab_e pos e1)
    | Binop (op, e1, e2) ->
        Binop (op, delab_e pos e1
                 , delab_e pos e2)
    | Cond (e1, e2, e3) ->
        Cond ( delab_e pos e1
             , delab_e pos e2
             , delab_e pos e3)
    | Lbl l ->
        Int (labref pos l)
  in
  let delab_c pos = function
    | Data i        -> Data i
    | Asgn (e1, e2) -> Asgn (delab_e pos e1, delab_e pos e2)
    | Cjmp (e1, e2) -> Cjmp (delab_e pos e1, delab_e pos e2)
    | Spwn e1       -> Spwn (delab_e pos e1)
  in
  src |> List.map snd
      |> List.mapi delab_c

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
  ; procs : proc Q.t
  }

let string_of_player p =
  let pcs =
    p.procs
      |> Q.to_list
      |> List.map string_of_proc
      |> String.concat " "
  in
  Printf.sprintf "%-5s | %02d : %s"
    (String.sub (p.name ^ "     ") 0 5)
    (int_of_color p.color)
    pcs

type state =
  mem * (player Q.t)

let string_of_queue q =
  q |> Q.to_list
    |> List.map string_of_player
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

(** Displays *)

type res =
  | Draw
  | Win of string

let string_of_res = function
  | Draw  -> "Draw"
  | Win s -> "Winner: " ^ s

type display =
  { init   : state -> unit
  ; write  : int * color * cell -> unit
  ; update : state -> unit
  ; finish : state * res -> unit
  }

let default_disp =
  { init   = ignore
  ; write  = ignore
  ; update = ignore
  ; finish = ignore
  }

let clear () =
  print_string "\027[2J";
  flush stdout

let debug_disp wait =
  { init   = ignore
  ; write  = ignore
  ; update = begin fun st ->
      clear ();
      st |> string_of_state
         |> print_endline;
      wait ()
    end
  ; finish = begin fun (_, res) ->
      res |> string_of_res
          |> print_endline
    end
  }

let step_disp =
  debug_disp (fun () -> ignore (read_line ()))

(* sleep for dur milliseconds *)
let nap dur =
  dur |> float_of_int
      |> flip (/.) 1000.0
      |> Unix.select [] [] []
      |> ignore

let test_disp dur =
  debug_disp (fun () -> nap dur)

(* h/t https://github.com/Chris00/ANSITerminal
 * and https://en.wikipedia.org/wiki/ANSI_escape_code
 *)
let term_disp w dur =
  let string_of_cc (color, cell) =
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
  let disp_mem mem =
    let aux i cc =
      if i mod w = 0 then begin
        (* move cursor to next line *)
        Printf.printf "\027[E"
      end;
      print_string (string_of_cc cc)
    in
    clear ();
    (* move cursor to top left *)
    Printf.printf "\027[H";
    Array.iteri aux mem;
    flush stdout
  in
  { init = begin fun (mem, players) ->
      (* disable cursor *)
      Printf.printf "\027[?25l";
      disp_mem mem
    end
  ; write = begin fun (addr, color, cell) ->
      let x = addr mod w + 1 in
      let y = addr  /  w + 2 in
      (* move cursor to (x, y) *)
      Printf.printf "\027[%d;%dH" y x;
      print_string (string_of_cc (color, cell));
      flush stdout;
      if dur > 0 then
        nap dur
    end
  ; update = ignore
  ; finish = begin fun ((mem, players), res) ->
      disp_mem mem;
      (* enable cursor *)
      Printf.printf "\027[?25h";
      Printf.printf "\n%s\n"
        (string_of_res res)
    end
  }

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
    ; procs = Q.single {pc = start}
    }
  in
  pps |> List.sort cmp_pp
      |> List.mapi aux
      |> List.rev
      |> Q.pushl Q.empty
      |> pair mem

(* cleaner than monads in OCaml *)
exception Bogus of string

let bogus msg =
  raise (Bogus msg)

let step_p disp mem color proc =
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
  let _cmp op a b =
    iob (op a b)
  in
  let rec eval_e = function
    | Int i ->
        Data i
    | Unop (op, a) ->
        evali_uop op a
    | Binop (op, a, b) ->
        evali_bop op a b
    | Cond (a, b, c) ->
        if boi (data (eval_e a))
        then eval_e b
        else eval_e c
    (* only for source programs *)
    | Lbl l ->
        bogus "tried to reference label"

  and evali_uop op a =
    let aux f =
      Data (f (data (eval_e a)))
    in
    match op with
    | Deref -> read (eval_e a)
    | Lnot  -> aux (lnot)
    | Neg   -> aux (~-)
    | Pos   -> aux (~+)
    | Not   -> aux (liftb_uop not)

  and evali_bop op a b =
    let aux f =
      Data (f (data (eval_e a))
              (data (eval_e b)))
    in
    match op with
    | Land -> aux (land)
    | Lor  -> aux (lor)
    | Lxor -> aux (lxor)
    | Lsl  -> aux (lsl)
    | Lsr  -> aux (lsr)
    | Add  -> aux (+)
    | Sub  -> aux (-)
    | Mul  -> aux ( * )
    | Div  -> aux _div
    | Mod  -> aux _mod
    | Lt   -> aux (_cmp (<))
    | Le   -> aux (_cmp (<=))
    | Gt   -> aux (_cmp (>))
    | Ge   -> aux (_cmp (>=))
    | Eq   -> Data (iob (eval_e a =  eval_e b))
    | Neq  -> Data (iob (eval_e a <> eval_e b))
    | Conj ->
        if boi (data (eval_e a))
        then Data (iob (boi (data (eval_e b))))
        else Data (iob false)
    | Disj ->
        if boi (data (eval_e a))
        then Data (iob true)
        else Data (iob (boi (data (eval_e b))))

  in
  let write c1 c2 =
    let addr =
      posmod (proc.pc + data c1)
             (Array.length mem)
    in
    store mem addr (color, c2);
    disp.write (addr, color, c2)
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
  | Spwn e1 ->
      [ {pc = proc.pc + 1}
      ; {pc = proc.pc + data (eval_e e1)}
      ]

let step disp (mem, players) =
  match Q.pop players with
  | None ->
      (* all players' procs crashed *)
      (mem, Q.empty)
  | Some (pl, pls) ->
      begin match Q.pop pl.procs with
      | None ->
          (* all pl's procs crashed *)
          (mem, pls)
      | Some (p, ps) ->
          try
            let ps' = step_p disp mem pl.color p in
            let pl' = {pl with procs = Q.pushl ps ps'} in
            (mem, Q.push pls pl')
          with Bogus msg -> (* p crashed *)
            let pl' = {pl with procs = ps} in
            (mem, Q.push pls pl')
      end

let draw (mem, players) =
  Q.is_empty players

let winner (mem, players) =
  match Q.pop players with
  | None -> None
  | Some (pl, pls) ->
      if Q.is_empty pls
      then if Q.is_empty pl.procs
           then None
           else Some pl.name
      else None

let run ?winstop:(winstop = true) disp tm st =
  disp.init st;
  let rec loop i st =
    match winner st, winstop with
    | Some nm, true ->
        (st, Win nm)
    | _, _ ->
        if i > tm || draw st then
          (st, Draw)
        else begin
          disp.update st;
          loop (i + 1) (step disp st)
        end
  in
  let (st', res) = loop 0 st in
  disp.finish (st', res)
