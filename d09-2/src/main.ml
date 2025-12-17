module Vec2 = struct
  type t = { x : int; y : int }

  let make x y = { x; y }

  let of_string_list lst =
    match lst with
    | [ x; y ] -> make (int_of_string x) (int_of_string y)
    | _ -> failwith "bad input"

  let area a b = (1 + Int.abs (a.x - b.x)) * (1 + Int.abs (a.y - b.y))
  let print (x : t) = Printf.printf "(%i %i)\n%!" x.x x.y

  let intersect (r1 : t) (r2 : t) (d1 : t) (d2 : t) : bool =
    r1 <> d1
    && r1 <> d2
    && r2 <> d1
    && r2 <> d2
    && max r1.x r2.x >= min d1.x d2.x
    && max r1.y r2.y >= min d1.y d2.y
    && min r1.x r2.x <= max d1.x d2.x
    && min r1.y r2.y <= max d1.y d2.y
end

let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

let parse_coords lines =
  List.map (String.split_on_char ',') lines |> List.map Vec2.of_string_list

let area_of_coords (coords : Vec2.t list) =
  let coords_1 = List.tl coords @ [ List.hd coords ] in
  let rec loop = function
    | p1 :: t ->
        let rec loop2 = function
          | p2 :: t when List.exists2 (Vec2.intersect p1 p2) coords coords_1 ->
              loop2 t
          | p2 :: t -> Vec2.area p1 p2 :: loop2 t
          | [] -> []
        in
        loop2 t @ loop t
    | [] -> []
  in
  loop coords

let () =
  read_lines Sys.argv.(1)
  |> parse_coords
  |> area_of_coords
  |> List.sort (fun a b -> -compare a b)
  |> List.hd
  |> Printf.printf "Res: %i\n"
