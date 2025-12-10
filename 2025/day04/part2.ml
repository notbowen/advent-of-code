open Aoc_utils.Utils

(* let pp_input = read_lines "./2025/day04/example.txt" *)
let pp_input = read_lines "./2025/day04/input.txt"
let max_x = String.length (List.nth pp_input 0) - 1
let max_y = List.length pp_input - 1
let input = pp_input |> String.concat "" |> explode

(* let print_list l = *)
(*   let rec aux l i = *)
(*     match l with *)
(*     | [] -> () *)
(*     | h :: t -> *)
(*         let () = print_char h in *)
(*         let () = if (i + 1) mod 10 = 0 then print_endline "" else () in *)
(*         aux t (i + 1) *)
(*   in *)
(*   aux l 0 *)

let dirs =
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

let count i pos dx dy =
  let x = pos mod (max_y + 1) in
  let y = pos / (max_y + 1) in

  let nx = x + dx in
  let ny = y + dy in

  if nx < 0 || nx > max_x || ny < 0 || ny > max_y then 0
  else
    let c = (ny * (max_y + 1)) + nx in
    if List.nth i c = '@' then 1 else 0

let rec solve l pos acc ol nl =
  match l with
  | [] -> (nl, acc)
  | h :: t ->
      if h = '@' then
        let nr =
          List.fold_left
            (fun acc dir ->
              let dx, dy = dir in
              acc + count ol pos dx dy)
            0 dirs
        in
        let d_acc = if nr < 4 then 1 else 0 in
        let c = if nr < 4 then '.' else '@' in
        solve t (pos + 1) (acc + d_acc) ol (nl @ [ c ])
      else solve t (pos + 1) acc ol (nl @ [ h ])

let rec main inpt acc =
  let nl, ac = solve inpt 0 0 inpt [] in
  (* let () = print_list nl in *)
  (* let () = Printf.printf "acc: %d\n\n" ac in *)
  if ac = 0 then acc else main nl (acc + ac)

let () = Printf.printf "%d\n" (main input 0)
