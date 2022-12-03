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