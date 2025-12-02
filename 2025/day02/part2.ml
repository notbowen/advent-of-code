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

(* Check if str is repeating *)
let rec count_str s m n =
  if n <= m then
    let replaced = Str.global_replace (Str.regexp (String.sub s 0 n)) "" s in
    (* let () = Printf.printf "%s\n" replaced in *)
    if String.length replaced == 0 then true else count_str s m (n + 1)
  else false

(* Process a given range of $s$ and $e$ *)
let rec process_range s e acc =
  if s > e then acc
  else
    let str = string_of_int s in
    let max_len = String.length str / 2 in

    if count_str str max_len 0 then process_range (s + 1) e (acc + s)
    else process_range (s + 1) e acc

let rec solve r acc =
  match r with
  | [] -> acc
  | h :: t ->
      let s, e = h in
      solve t (process_range s e acc)

let () = Printf.printf "%d\n" (solve (parse_ranges input) 0)
