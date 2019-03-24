(* https://www.codewars.com/kata/561e9c843a2ef5a40c0000a4/train/ocaml *)




let gap gapspace start stop = 
  let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
    n <> 1 && is_not_divisor 2 in
  let are_both_prime start stop = (is_prime start) && (is_prime stop) in
  let (--) i j = 
    let rec aux n acc = 
      if n < i then acc else aux (n-1) (n::acc)
    in aux j [] in
  let any_prime_in_range list = List.exists is_prime list in
  let rec aux start interstop =
    match(start, interstop) with
      (_, stp) when stp > stop-> None
    | (start, stop) when (are_both_prime start stop) && not(any_prime_in_range ((start+1)--(stop-1))) ->  Some(start, stop)
    | (oldstart, _) -> aux (oldstart+1) (oldstart+1+gapspace)
  in 
  aux start (start+gapspace)

