let _ =
  Random.self_init ()

let (|>) x f = f x

let (>>) x f = ignore (f x); x

let curry f (a, b) = f a b

let uncurry f a b = f (a, b)

let flip f a b = f b a

let pair a b = (a, b)

(* not allowed to do (::) *)
let cons a b = a :: b

let range n =
  let rec loop i acc =
    if i < 0
    then acc
    else loop (i - 1) (i :: acc)
  in
  loop (n - 1) []

let posmod i n =
  ((i mod n) + n) mod n

let boi i =
  if i = 0
  then false
  else true

let iob = function
  | false -> 0
  | true  -> 1

let liftb_uop op i =
  iob (op (boi i))

let liftb_bop op i1 i2 =
  iob (op (boi i1)
          (boi i2))

let with_ic path f =
  let ic = Pervasives.open_in path in
  let res = f ic in
  Pervasives.close_in ic;
  res

let with_oc path f =
  let oc = Pervasives.open_out path in
  let res = f oc in
  Pervasives.close_out oc;
  res

let of_file_lines nm =
  with_ic nm (fun ic ->
    let rec loop ls =
      let next =
        try Some (Pervasives.input_line ic)
        with End_of_file -> None
      in
      match next with
      | None   -> List.rev ls
      | Some l -> loop (l :: ls)
    in
    loop [])

let of_file nm =
  nm |> of_file_lines
     |> String.concat "\n"

let to_file nm s =
  with_oc nm (fun oc ->
    Pervasives.output_string oc s)
