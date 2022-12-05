let line_stream_of_channel channel =
  Seq.of_dispenser
    (fun _ ->
      try Some (input_line channel) with End_of_file -> None)

let read_file fname = 
  let in_ch = open_in fname in
  try 
    line_stream_of_channel in_ch
  with e ->
    close_in in_ch;
    raise e

let split_list list n = 
  let rec split list1 list2 n =
    if List.length list1 = n then
      [List.rev list1; list2]
  else
    match list1 with
      | x :: xs -> split (xs) (x :: list2) n
      | [] -> [[]; []] in

  split (List.rev list) [] n