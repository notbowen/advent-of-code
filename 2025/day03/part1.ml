open Aoc_utils.Utils

let input = read_lines "./2025/day03/input.txt"
let rint_of_char c = int_of_char c - 48
let lint_of_lchar l = List.map rint_of_char l

let rec max_jolt_inner d l m =
  match l with
  | [] -> m
  | h :: t ->
      let mx = max m ((d * 10) + h) in
      max_jolt_inner d t mx

let rec max_jolt l m =
  match l with [] -> m | h :: t -> max_jolt t (max_jolt_inner h t m)

let rec solve i acc =
  match i with
  | [] -> acc
  | h :: t ->
      let e = explode h in
      let l = lint_of_lchar e in
      let m = (List.nth l 0 * 10) + List.nth l 1 in
      solve t (acc + max_jolt l m)

let () = Printf.printf "%d\n" (solve input 0)
