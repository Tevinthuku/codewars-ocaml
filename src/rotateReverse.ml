(* https://www.codewars.com/kata/reverse-or-rotate/ocaml *)

let reverseString  str = 
  let rec aux str idx limit acc = 
    if idx == limit then acc 
    else aux str (idx+1) limit acc ^ (String.make 1 str.[idx])
  in aux str 0 (String.length str) ""


let rotateString str = 
  let rec aux str idx limit acc = 
    if idx == limit then (acc ^ (String.make 1 str.[0]))
    else aux str (idx+1) limit (acc ^ (String.make 1 str.[idx]))
  in aux str 1 (String.length str) ""


let power a b = 
  let rec aux a b idx acc = 
    if b == idx then acc
    else aux a b (idx+1) (acc * a) in
  aux a b 0 1


let isCubeSumDivisibleByTwo str = 
  let rec aux str endlimit idx acc = 
    if idx == endlimit then acc
    else aux str endlimit (idx+1) (acc + (power (int_of_string (String.make 1 str.[idx])) 3))
  in (aux str (String.length str) 0 0) mod 2 == 0

let resolveChunkFunction str = match (isCubeSumDivisibleByTwo str) with
    | true -> reverseString str
    | _ -> rotateString str

let rec concat xs = match xs with
  | [] -> ""
  | h :: t -> h ^ concat t


let getChunks limit str sz = 
  let rec aux str initialSz sz idx limitidx chunk lst = 
    if limit == limitidx then lst
    else if idx == sz then aux str initialSz (sz+initialSz) (idx) (limitidx+1) "" (chunk::lst)
    else aux str initialSz sz (idx+1) (limitidx) (chunk ^ (String.make 1 (String.get str idx))) lst
  in aux str sz sz 0 0 "" [] |> List.rev |> List.map resolveChunkFunction



  
let revrot str sz =
  let isStringLengthSmaller str sz = (String.length str) < sz in
  let aux str sz = if isStringLengthSmaller str sz then "" else (getChunks ((String.length str) / sz) str sz |> concat) in
  let ans str sz = match (str, sz) with
    ("", _) -> ""
    |(_, 0) -> ""
    | (_, _) -> aux str sz
  in ans str sz
