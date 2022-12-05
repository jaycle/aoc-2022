module CharSet = Set.Make(Char);;
exception Invalid_input;;

let () =
  let score_char ch = 
    let upper = int_of_char 'A' in
    let lower = int_of_char 'a' in
    let ch_val = int_of_char ch in
    if ch_val >= lower then 
      ch_val - lower + 1
    else
      ch_val - upper + 27
    in

  let split_str s =
    let len = String.length s in 
    let adder = if len mod 2 = 0 then 0 else 1 in (* Not used, but good idea *)
    (String.sub s 0 (len / 2), String.sub s (len / 2) ((len / 2) + adder)) in
  
  let to_set s = 
    let seq = String.to_seq s in 
    Seq.fold_left
      (fun set c -> CharSet.add c set)
      CharSet.(empty)
      seq in

  let score s =
    let comps = split_str s in 
    let sets = (to_set (fst comps), to_set (snd comps)) in
    let common = CharSet.elements @@ CharSet.inter (fst sets) (snd sets) in
    if List.length common > 1 then
      raise Invalid_input
    else
      score_char @@ List.hd common in

  let lines = Seq.fold_left 
    (fun acc l -> l :: acc)
    []
    (Lib.Files.read_file "day3.txt") in
  
  (* Pt. 1 *)
  let pt1 = fun x -> x |> List.map (fun l -> (score l)) |> List.fold_left (+) 0 in
  let _ = print_endline @@ string_of_int @@ pt1 lines in

  (* Pt. 2 *)
  let partition_by n acc s = 
    match acc with
      | h :: rest -> 
        if List.length h = n then
          [s] :: h :: rest
        else
          (s :: h) :: rest
      | [] -> [[s]] in
  let paritioned = List.fold_left (partition_by 3) [] lines in
  let score2 l =
    let sets = List.map to_set l in
    let common = CharSet.elements @@ List.fold_left CharSet.inter (List.hd sets) (List.tl sets) in
    if List.length common = 1 then
      score_char @@ List.hd common
    else
      raise Invalid_input in

  let pt2 = fun x -> x |> List.map score2 |> List.fold_left (+) 0 in
  let _ = print_endline @@ string_of_int @@ pt2 paritioned in
  ()