open Aoc_utils.Utils

let input = List.hd (read_lines "./2025/day02/input.txt")
(* let input = List.hd (read_lines "./2025/day02/example.txt") *)

(* Parse a singular range *)
let parse_range r =
  let ranges = Str.split (Str.regexp "-") r in
  let left = int_of_string (List.nth ranges 0) in
  let right = int_of_string (List.nth ranges 1) in
  (left, right)

(* Parse ranges into (int * int) list *)
let parse_ranges r =
  let ranges = Str.split (Str.regexp ",") r in
  List.map parse_range ranges

(* Calculate len of number *)
let len i = int_of_float (log (float_of_int i) /. log 10.0) + 1

(* Check range for invalid IDs *)
let rec check_range s e acc =
  if s > e then acc
  else
    let l = len s in
    if l mod 2 == 0 then
      let r = int_of_float (10.0 ** (float_of_int l /. 2.0)) in
      let a = s / r in
      let b = s - (a * r) in
      check_range (s + 1) e (if a == b then acc + s else acc)
    else check_range (s + 1) e acc

let rec solve r acc =
  match r with
  | [] -> acc
  | h :: t ->
      let s, e = h in
      solve t (check_range s e acc)

let () = Printf.printf "%d\n" (solve (parse_ranges input) 0)
