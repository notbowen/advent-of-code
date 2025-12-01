let read_lines filename =
  let ch = open_in filename in
  let rec loop acc =
    try loop (input_line ch :: acc)
    with End_of_file ->
      close_in ch;
      List.rev acc
  in
  loop []
