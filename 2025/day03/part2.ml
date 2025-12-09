open Aoc_utils.Utils

let input = read_lines "./2025/day03/input.txt"
(* let input = read_lines "./2025/day03/example.txt" *)
let explode j = List.init (String.length j) (String.get j)
let int_of_raw_char c = int_of_char c - int_of_char '0'

let rec pow b e =
  if e < 0 then failwith "e < 0"
  else if e = 0 then 1
  else if e mod 2 = 0 then
    let h = pow b (e / 2) in
    h * h
  else b * pow b (e - 1)

let rec int_of_stack s acc i =
  if Stack.is_empty s then
    acc
  else
    let top_char = Stack.pop s in
    let top_int = int_of_raw_char top_char in
    int_of_stack s (acc + (top_int * pow 10 i)) (i + 1)

let rec process_pop s r h =
  if r > 0 && (not (Stack.is_empty s)) && h > Stack.top s then
    let _ = Stack.pop s in
    process_pop s (r - 1) h
  else r

let rec calc_jolt s j r =
  match j with
  | [] ->
      while Stack.length s > 12 do
        ignore (Stack.pop s)
      done;
      int_of_stack s 0 0
  | h :: t ->
      let nr = process_pop s r h in
      Stack.push h s;
      calc_jolt s t nr

let rec solve i acc =
  match i with
  | [] -> acc
  | h :: t ->
      let j = explode h in
      let s = Stack.create () in
      solve t (acc + calc_jolt s j (List.length j - 12))

let () = Printf.printf "%d\n" (solve input 0)
