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
  let memo : (bool * bool * string, int) Hashtbl.t = Hashtbl.create 1024 in
  let rec do_node has_dac has_fft node =
    match Hashtbl.find_opt table node with
    | Some next_nodes ->
        List.map (loop has_dac has_fft) next_nodes |> List.fold_left ( + ) 0
    | None -> failwith "node not found"
  and loop has_dac has_fft node =
    match Hashtbl.find_opt memo (has_dac, has_fft, node) with
    | Some v -> v
    | None ->
        let result =
          match node with
          | "out" when has_dac && has_fft -> 1
          | "out" -> 0
          | "dac" -> do_node true has_fft node
          | "fft" -> do_node has_dac true node
          | node -> do_node has_dac has_fft node
        in
        Hashtbl.add memo (has_dac, has_fft, node) result;
        result
  in
  loop false false "svr"

let () =
  read_lines Sys.argv.(1)
  |> table_of_lines
  |> process
  |> Printf.printf "res: %i\n%!"
