type hand = Paper | Rock | Scissors
type result = Win | Loss | Draw

let () =
  let map_hand = function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> Rock in

  let rules = function
    | (Scissors, Rock)
    | (Paper, Scissors)
    | (Rock, Paper) -> Win
    | (h1, h2) when h1 = h2 -> Draw
    | _ -> Loss in

  let score (h: hand) (r: result) =
    let h_score = match h with
      | Rock -> 1
      | Paper -> 2
      | Scissors -> 3 in
    let r_score = match r with
      | Win -> 6
      | Draw -> 3
      | Loss -> 0 in
      h_score + r_score in

  let hands_of_line line = 
    let chars = String.split_on_char ' ' line in
    (map_hand @@ List.nth chars 0, map_hand @@ List.nth chars 1) in

  let scores hands = 
    let res = rules hands in
    score (snd hands) res in

  let lines = Seq.fold_left 
    (fun acc l -> l :: acc)
    []
    (Files.read_file "day2.txt") in

  (* Pt 1 *)
  let pt1 = List.map (fun x -> x |> hands_of_line |> scores) in
  let _ = print_endline @@ string_of_int @@ List.fold_left (+) 0 (pt1 lines) in

  (* Pt 2 *)
  let map_result = function
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" | _ -> Win in

  (* Lazy solution *)
  let hand_to_play = function
    | (h, Draw) -> h
    | (h, Win) -> (match h with | Rock -> Paper | Paper -> Scissors | Scissors -> Rock)
    | (h, Loss) -> (match h with | Rock -> Scissors | Scissors -> Paper | Paper -> Rock) in
  
  let result_for_line line =
    let chars = String.split_on_char ' ' line in 
    let theirs = map_hand @@ List.nth chars 0 in
    let expected =  map_result @@ List.nth chars 1 in
    (theirs, hand_to_play (theirs, expected)) in

  let pt2 = List.map (fun x -> x |> result_for_line |> scores) in
  print_endline @@ string_of_int @@ List.fold_left (+) 0 (pt2 lines)