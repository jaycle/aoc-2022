module CharMap = Map.Make(Char)

module Ring : sig
  type 'a t
  val of_list : 'a list -> 'a t
  val shift : 'a -> 'a t -> 'a * 'a t
end = struct
  type 'a t = {
    ring: 'a array;
    hd: int;
  }

  let of_list a = { 
    ring = Array.of_list a;
    hd = 0;
  }

  let shift v r =
    let ret = Array.get r.ring r.hd in
    let new_ring = match r with
      | { ring; hd } -> 
        ring.(hd) <- v;
        {ring; hd = (hd + 1) mod Array.length ring} in
    (ret, new_ring)
end

let find_unique size seq =
  let split = Lib.Files.split_list seq size in
  let (first, rest) = match split with
    | x :: xs -> (x, List.hd xs)
    | [] -> ([], []) in

  let add_entry = function
    | Some v -> Some (v + 1)
    | None -> Some 1 in 

  let remove_entry = function
    | Some v when v > 1 -> Some (v - 1)
    | None | Some _ -> None in

  let update_map map remove add =
    if remove = add then
      map
    else
      let map = CharMap.update add add_entry map in
      CharMap.update remove remove_entry map in

  let map = List.fold_left
    (fun m v -> CharMap.update v add_entry m)
    CharMap.empty
    first in

  let rec walk b map l i =
    if CharMap.cardinal map = size then
      i
    else
      match l with
        | x :: xs -> 
          let old, b = Ring.shift x b in
          let map = update_map map old x in
          walk b map xs i + 1
        | [] -> i in
          
  walk (Ring.of_list first) map rest size

let lines = Seq.fold_left 
  (fun acc l -> l :: acc)
  []
  (Lib.Files.read_file "day6.txt")

let char_arr = (match lines with x :: _ -> String.to_seq x | _ -> Seq.empty) |> List.of_seq

(* Pt1 *)
let _ = print_endline @@ string_of_int @@ find_unique 4 char_arr

(* Pt2 *)
let _ = print_endline @@ string_of_int @@ find_unique 14 char_arr