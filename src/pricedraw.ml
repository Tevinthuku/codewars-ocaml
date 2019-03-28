(* https://www.codewars.com/kata/5616868c81a0f281e500005c/train/ocaml *)

(* run this file with utop I will be switching to esy soon. *)
let rank (st: string) (we: int array) (n: int): string =
  let word_sum s w = 
    let lower_case_str s = String.lowercase s
    in
    let letter_rank c = (int_of_char c) - 96
    in
    (String.length s +
     ((lower_case_str s) 
      |> Str.split (Str.regexp "") 
      |> List.map(fun x -> letter_rank x.[0]) 
      |> List.fold_left (+) 0)) * w
  in

  let comparison (sum1, name1) (sum2, name2) =
    let cp =  sum2 - sum1 in
    let cp1 = compare name1 name2 in
    if cp = 0 then cp1 else cp
  in
  if (st = "") then "No participants"
  else if (Array.length we < n) then "Not enough participants"
  else
    let l = Str.split (Str.regexp ",") st |> Array.of_list in
    let a = Array.mapi(fun i x -> word_sum x we.(i), x) l in
    Array.sort comparison a;
    snd a.(n - 1)

let a = rank  "Sophia,Robert,Abigail,Grace,Lagon" [|1;2;2;6;4|] 3
let b = rank  "Addison,Jayden,Sofia,Michael,Andrew,Lily,Benjamin" [|4;2;1;4;3;1;2|] 8              
