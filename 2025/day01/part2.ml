open Aoc_utils.Utils

let input = read_lines "./2025/day01/input.txt"
(* let input = read_lines "./2025/day01/example.txt" *)

(* Parse rotor instruction *)
let parse_line s =
  let first = s.[0] in
  let rest = String.sub s 1 (String.length s - 1) in
  let c =
    match first with
    | 'L' -> -1
    | 'R' -> 1
    | _ -> failwith "wattesigma is this op"
  in
  c * int_of_string rest

let wrap n =
  let r = n mod 100 in
  if r < 0 then r + 100 else r

let rec solve lines rotor ans =
  match lines with
  | [] -> ans
  | h :: t ->
      let delta = parse_line h in

      let add =
        if delta > 0 then (rotor + delta) / 100
        else
          let mag = -delta in
          let d = if rotor = 0 then 100 else rotor in

          if mag >= d then 1 + ((mag - d) / 100) else 0
      in

      let new_rotor = wrap (rotor + delta) in
      solve t new_rotor (ans + add)

let () = print_endline (string_of_int (solve input 50 0))
