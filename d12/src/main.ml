let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

let rec map_until_break fn lines =
  let rec loop lines =
    match lines with
    | h :: t when h <> "" -> fn h :: loop t
    | _ -> []
  in
  loop lines

let process (line : string) =
  match String.split_on_char ':' line |> List.map String.trim with
  | [ h; t ] ->
      let size =
        String.split_on_char 'x' h
        |> List.map int_of_string
        |> List.fold_left ( * ) 1
      in
      let shapes_area =
        String.split_on_char ' ' t
        |> List.map int_of_string
        |> List.fold_left ( + ) 0
      in
      size >= shapes_area * 9
  | _ -> failwith "invalid_arg"

let () =
  read_lines Sys.argv.(1)
  |> List.rev
  |> map_until_break process
  |> List.fold_left (fun acc x -> acc + if x then 1 else 0) 0
  |> Printf.printf "%i\n"
