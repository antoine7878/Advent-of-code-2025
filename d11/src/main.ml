let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

type adj = (string, string list) Hashtbl.t

let table_of_lines lines =
  let table : adj = Hashtbl.create 1024 in
  List.iter
    (fun line ->
      match String.split_on_char ':' line with
      | [ node; edge ] ->
          Hashtbl.add table node
            (edge |> String.trim |> String.split_on_char ' ')
      | _ -> failwith "ouspi")
    lines;
  table

let process (table : adj) =
  let memo : (string, int) Hashtbl.t = Hashtbl.create 1024 in
  let rec loop node =
    match Hashtbl.find_opt memo node with
    | Some v -> v
    | None ->
        let result =
          if node = "out" then 1
          else
            match Hashtbl.find_opt table node with
            | Some node -> List.fold_left (fun acc x -> acc + loop x) 0 node
            | None -> 0
        in
        Hashtbl.add memo node result;
        result
  in
  loop "you"

let () =
  read_lines Sys.argv.(1)
  |> table_of_lines
  |> process
  |> Printf.printf "res: %i\n%!"
