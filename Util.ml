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
