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

(* 11: modified run-length encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode2 l =
  let rec scan acc last counter =
    function
    | x::xs when x = last -> (scan [@tailcal]) acc last (counter + 1) xs
    | l' ->
      let record = Many (counter, last) in
      (restart [@tailcal]) (record::acc) l'
  and restart acc =
    function
    | x::y::xs when x = y -> (scan [@tailcal]) acc x 2 xs
    | x::xs -> (restart [@tailcal]) ((One x)::acc) xs
    | [] -> acc
  in 
  restart [] l |> List.rev

(* 12: decode a run-length encoded list *)
let decode l =
  let rec aux acc =
    function
    | [] -> acc
    | (One x)::tail -> (aux [@tailcal]) (x::acc) tail
    | (Many (n, x))::tail -> (aux [@tailcal]) (repeat acc x n) tail
  and repeat acc x n =
    if n > 0
    then (repeat [@tailcal]) (x::acc) x (n - 1)
    else if n = 0
    then acc
    else assert false
  in
  aux [] @@ List.rev l

(* 13: run-length encoding of a list *)
(* UNCLEAR: how is it different from 11? *)

(* 14: duplicate the elements of a list *)
let duplicate l =
  let rec aux acc =
    function
    | [] -> acc
    | x::xs -> (aux [@tailcal]) (x::x::acc) xs
  in
  aux [] @@ List.rev l

(* 15: replicate the elements of a list a given number of times *)
let replicate l n_times =
  let rec repeat acc ?(n=n_times) x =
    if n > 0
    then (repeat [@tailcal]) (x::acc) x ~n:(n - 1)
    else if n = 0
    then acc
    else assert false
  in
  let rec aux acc =
    function
    | [] -> acc
    | x::xs -> (aux [@tailcal]) (repeat acc x) xs
  in
  aux [] @@ List.rev l

(* 16: drop every n-th element from a list *)
let drop list n =
  assert (n > 0);
  let rec aux acc k l =
    match k + 1, l with
    | _, [] -> acc
    | k', x::xs when k' = n -> (aux [@tailcal]) acc 0 xs
    | k', x::xs -> (aux [@tailcal]) (x::acc) k' xs
  in
  aux [] 0 list |> List.rev

(* 17: split a list into two parts; the length of the first part is given *)
let split list n =
  assert (n >= 0);
  let rec aux acc k l =
    match k, l with
    | 0, tail
    | _, ([] as tail) -> List.rev acc, tail
    | _, x::xs -> (aux [@tailcal]) (x::acc) (k - 1) xs
  in
  aux [] n list

(* 18: extract slice from a list *)
let slice list i k =
  assert (i >= 0);
  assert (k >= i);
  let rec aux acc n l =
    match n, l with
    | n, x::xs when n < i -> (aux [@tailcal]) acc (n + 1) xs
    | n, x::xs when n <= k -> (aux [@tailcal]) (x::acc) (n + 1) xs
    | _ -> List.rev acc
  in
  aux [] 0 list

