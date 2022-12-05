type pairs = 
  {
    one: (int * int);
    two: (int * int);
  }

let to_ranges line = Scanf.sscanf line "%d-%d,%d-%d" (fun a b c d -> {one = (a, b); two = (c, d)})

let is_contained (p : pairs) =
  fst p.one <= fst p.two && snd p.two <= snd p.one || (* one encloses two *)
  fst p.two <= fst p.one && snd p.one <= snd p.two    (* two encloses one *)

let lines = Seq.fold_left 
  (fun acc l -> l :: acc)
  []
  (Lib.Files.read_file "day4.txt")

(* Pt 1 *)
let contained_count = lines |> List.map to_ranges |> List.filter is_contained |> List.length
let _ = print_endline (string_of_int contained_count)

(* Pt 2 *)
let is_within (p : pairs) =
  is_contained p ||
  fst p.two <= snd p.one && snd p.one <= snd p.two || (* two crosses one's upper boundary *)
  fst p.two <= fst p.one && fst p.one <= snd p.two    (* two crosses one's lower boundary *)

let overlapping_count = lines |> List.map to_ranges |> List.filter is_within |> List.length
let _ = print_endline (string_of_int overlapping_count)