(* https://www.codewars.com/kata/5541f58a944b85ce6d00006a/train/ocaml *)

let fibonacci number =
  let rec aux index current accumulator =
      if index = number then accumulator
      else aux (index + 1) accumulator (current + accumulator)
  in
  aux 0 0 1

let getFibonacciOfCurrentAndNext number = (fibonacci number, fibonacci (number+1))

let product_fib number = 
  let rec aux current = 
    let (currentFibonacci, nextFibonacci) = getFibonacciOfCurrentAndNext current in
    if currentFibonacci * nextFibonacci == number then (currentFibonacci, nextFibonacci , 1)
    else if currentFibonacci * nextFibonacci > number then (currentFibonacci, nextFibonacci , 0)
    else aux (current+1) in
  aux 0

