let () = 
  let acc = fun accum l -> l :: accum in
  let folded = Seq.fold_left acc [] (Files.read_file "day1.txt" ) in
  let lines = List.rev folded in
  let sum_cals l = function
    | "" -> 0 :: l
    | v -> match l with 
      | [] -> [int_of_string v]
      | last :: rest -> int_of_string v + last :: rest in

  let summed = List.fold_left sum_cals [] lines in
  let sorted = List.sort
    (fun a b -> b - a)
    summed in

  (* Pt. 1 *)
  let _ = sorted |> List.hd |> string_of_int |> print_endline in

  (* Pt. 2 *)
  let rec top3 ?(i=0) ?(sum=0) () = match i with
    | i when i < 3 -> top3 ~i:(i + 1) ~sum:((List.nth sorted i) + sum) ()
    | _ -> sum in
  
  top3 () |> string_of_int |> print_endline