open Aoc_utils.Utils

let input = read_lines "./2025/day01/input.txt"
(* let input = read_lines "./2025/day01/example.txt" *)

let parse_str s =
  let first = s.[0] in
  let rest = String.sub s 1 (String.length s - 1) in
  (first, int_of_string rest)

let calc_new s n =
  let dir, times = parse_str s in
  let op =
    match dir with
    | 'R' -> ( + )
    | 'L' -> ( - )
    | _ -> failwith "wattesigma is this dir"
  in
  let new_n = op n times in
  (* let () = print_endline (s ^ " " ^ string_of_int (new_n mod 100)) in *)
  new_n mod 100

let rec solve lines acc ans =
  match lines with
  | [] -> ans
  | h :: t ->
      let n = calc_new h acc in
      solve t n (if n == 0 then ans + 1 else ans)

let () = print_endline (string_of_int (solve input 50 0))
