module Vec3 = struct
  type t = { x : int; y : int; z : int }

  let make x y z = { x; y; z }

  let of_string_list lst =
    match lst with
    | [ x; y; z ] -> make (int_of_string x) (int_of_string y) (int_of_string z)
    | _ -> failwith "bad input"

  let distance_2 a b =
    ((a.x - b.x) * (a.x - b.x))
    + ((a.y - b.y) * (a.y - b.y))
    + ((a.z - b.z) * (a.z - b.z))

  let print (x : t) = Printf.printf "(%i %i %i)\n%!" x.x x.y x.z
  let min (a : t) (b : t) = if b.z > a.z then b else a
  let compare_z a b = compare a.z b.z
end

module Matrix = struct
  type t = Vec3.t list

  let of_coords_list (coords : Vec3.t list) =
    let rec loop i = function
      | h :: t ->
          List.mapi (fun j y -> Vec3.make i (i + j + 1) (Vec3.distance_2 h y)) t
          :: loop (i + 1) t
      | [] -> []
    in
    loop 0 coords |> List.flatten

  let min (matrix : t) = List.fold_left Vec3.min (List.hd matrix) matrix
end

let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim |> String.split_on_char '\n'

let parse_coords lines =
  List.map (String.split_on_char ',') lines |> List.map Vec3.of_string_list

module IntSet = Set.Make (Int)

let rec merge_set_list sets =
  let rec loop sets =
    match sets with
    | h :: t ->
        let not_merge, merge = List.partition (IntSet.disjoint h) sets in
        List.fold_left IntSet.union h merge :: loop not_merge
    | [] -> []
  in
  let new_sets = loop sets in
  if List.compare_lengths new_sets sets = 0 then sets
  else merge_set_list new_sets

let get_last_edge count (edges : Vec3.t list) : Vec3.t =
  let rec loop sets (edges : Vec3.t list) =
    match edges with
    | h :: t -> (
        let sets = merge_set_list (IntSet.of_list [ h.x; h.y ] :: sets) in
        match sets with
        | [ s ] when IntSet.cardinal s = count -> h
        | _ -> loop sets t)
    | [] -> failwith "not solution"
  in
  loop [] edges

let () =
  let nodes = read_lines Sys.argv.(1) |> parse_coords in
  let node_count = List.length nodes in
  let last_edge =
    nodes |> Matrix.of_coords_list |> List.sort Vec3.compare_z
    |> get_last_edge node_count
  in
  Printf.printf "res: %i\n"
    ((List.nth nodes last_edge.x).x * (List.nth nodes last_edge.y).x)
