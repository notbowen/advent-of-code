open Aoc_utils.Utils

(* let pp_input = read_lines "./2025/day04/example.txt" *)
let pp_input = read_lines "./2025/day04/input.txt"
let max_x = String.length (List.nth pp_input 0) - 1
let max_y = List.length pp_input - 1
let input = pp_input |> String.concat "" |> explode

let dirs =
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

let count pos dx dy =
  let x = pos mod (max_y + 1) in
  let y = pos / (max_y + 1) in

  let nx = x + dx in
  let ny = y + dy in

  if nx < 0 || nx > max_x || ny < 0 || ny > max_y then 0
  else
    let c = (ny * (max_y + 1)) + nx in
    if List.nth input c = '@' then 1 else 0

let rec solve l pos acc =
  match l with
  | [] -> acc
  | h :: t ->
      if h = '@' then
        let nr =
          List.fold_left
            (fun acc dir ->
              let dx, dy = dir in
              acc + count pos dx dy)
            0 dirs
        in
        let d_acc = if nr < 4 then 1 else 0 in
        solve t (pos + 1) (acc + d_acc)
      else solve t (pos + 1) acc

let () = Printf.printf "%d\n" (solve input 0 0)
