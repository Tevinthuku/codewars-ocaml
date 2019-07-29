(* https://www.codewars.com/kata/reverse-or-rotate/ocaml *)

let getChar str idx = str.[idx]

let makeStringFromSingleChar chr = String.make 1 chr


let getStr ?(idx=0) str = (makeStringFromSingleChar (getChar str idx))

let reverseString  str = 
  let rec aux str idx limit acc = 
    if idx == limit then acc 
    else aux str (idx+1) limit acc ^ (getStr str ~idx:idx)
  in aux str 0 (String.length str) ""


let rotateString str = 
  let rec aux str idx limit acc = 
    if idx == limit then (acc ^ (getStr str))
    else aux str (idx+1) limit (acc ^ (getStr str ~idx:idx))
  in aux str 1 (String.length str) ""


let power a b = 
  let rec aux a b idx acc = 
    if b == idx then acc
    else aux a b (idx+1) (acc * a) in
  aux a b 0 1


let isCubeSumDivisibleByTwo str = 
  let rec aux str endlimit idx acc = 
    if idx == endlimit then acc
    else aux str endlimit (idx+1) (acc + (power (int_of_string (getStr str ~idx:idx))) 3)
  in (aux str (String.length str) 0 0) mod 2 == 0

let resolveChunkAction str = match (isCubeSumDivisibleByTwo str) with
    | true -> reverseString str
    | _ -> rotateString str

let rec concat xs = match xs with
  | [] -> ""
  | h :: t -> h ^ concat t


let getChunks limit str sz = 
  let rec aux str initialSz sz idx limitidx chunk lst = 
    if limit == limitidx then lst
    else if idx == sz then aux str initialSz (sz+initialSz) (idx) (limitidx+1) "" (chunk::lst)
    else aux str initialSz sz (idx+1) (limitidx) (chunk ^ (getStr str ~idx:idx)) lst
  in aux str sz sz 0 0 "" [] |> List.rev |> List.map resolveChunkAction

  
let isStringLengthSmallerThanSize str sz = (String.length str) < sz

let revrot str sz =
  let res str sz = if isStringLengthSmallerThanSize str sz then "" 
                  else (getChunks ((String.length str) / sz) str sz |> concat) in
  let aux str sz = match (str, sz) with
     ("", _) -> ""
    |(_, 0) -> ""
    | (_, _) -> res str sz
  in aux str sz
