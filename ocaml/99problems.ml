(* from: https://ocaml.org/exercises *)

(* 1: last element of a list*)
let rec last =
  function
  | [] -> None
  | [x] -> Some x
  | _::xs -> xs |> last [@tailcall]

(* 2: last two elements of a list*)
let rec last_two =
  function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _::xs -> xs |> last_two [@tailcall]


(* 3: n'th element of a list*)
let rec at_bad n l =
  match n, l with
  | _, [] -> None
  | 0, x::_ -> Some x
  | n, _::xs when n > 0 -> (at [@tailcall]) (n - 1) xs
  | n, l ->
    assert (n < 0);
    (* bad: O(n) in memory *)
    let from_end (type a) (l : a list) = 
      let exception Found of a in
      let rec aux =
        function
        | [] -> -1
        | x::xs ->
          let k = aux xs in
          if k = n then raise @@ Found x;
          k - 1
      in
      match aux l with
      | exception Found ans -> Some ans
      | _ -> None
    in from_end l

let rec at n l =
  match n, l with
  | _, [] -> None
  | 0, x::_ -> Some x
  | n, _::xs when n > 0 -> (at [@tailcall]) (n - 1) xs
  | n, l ->
    assert (n < 0);
    match n + List.length l with
    | n' when n' >= 0 -> at n' l
    | _ -> None

(* 4: length of a list *)
let length =
  let rec aux start =
    function
    | [] -> start
    | _::xs -> (aux [@tailcall]) (start + 1) xs
  in
  aux 0

(* 5: reverse a list *)
let rev =
  let rec aux acc =
    function
    | [] -> acc
    | x::xs -> (aux [@tailcall]) (x::acc) xs
  in
  aux []

(* 6: palindrome *)
let is_palindrome l =
  let rec aux n acc =
    (* reverse of only half list *)
    function
    | tail when n = 0 -> acc = tail
    | _::tail when n = 1 -> acc = tail
    | x::xs -> (aux [@tailcal]) (n - 2) (x::acc) xs
    | _ -> assert false
  in
  aux (List.length l) [] l

(* 7: flatten a list *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten l =
  let rec aux acc =
    function
    | [] -> acc
    | (One x) :: tail -> (aux [@tailcal]) (x::acc) tail
    | (Many xs) :: tail -> (aux [@tailcal]) (aux acc xs) tail
  in
  aux [] l |> List.rev

(* 8: eliminate duplicates *)
let compress =
  let rec aux acc last =
    function
    | [] -> acc
    | x::xs when x = last -> (aux [@tailcal]) acc last xs
    | x::xs -> (aux [@tailcal]) (x::acc) x xs
  in
  function
  | x::xs -> aux [x] x xs |> List.rev
  | [] -> []

(* 9: pack consecutive duplicates *)
let pack =
  let rec aux acc last run =
    function
    | [] -> run::acc
    | x::xs when x = last -> (aux [@tailcal]) acc last (x::run) xs
    | x::xs -> (aux [@tailcal]) (run::acc) x [x] xs
  in
  function
  | x::xs -> aux [] x [x] xs |> List.rev
  | [] -> []

(* 10: run-length encoding *)
let encode =
  let rec aux acc last run =
    function
    | [] -> (run, last)::acc
    | x::xs when x = last -> (aux [@tailcal]) acc last (run + 1) xs
    | x::xs -> (aux [@tailcal]) ((run, last)::acc) x 1 xs
  in
  function
  | x::xs -> aux [] x 1 xs |> List.rev
  | [] -> []
