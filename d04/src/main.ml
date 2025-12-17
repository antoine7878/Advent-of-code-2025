let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

let print_matrix matrix =
  let print_array vec =
    Array.iter print_int vec;
    print_newline ()
  in
  Array.iter print_array matrix

let matrix_of_lines lines =
  let i_max = List.length lines in
  let j_max = String.length (List.hd lines) in
  let matrix = Array.make_matrix i_max j_max '!' in
  let rec loop i = function
    | h :: t ->
        for j = 0 to j_max - 1 do
          matrix.(i).(j) <- h.[j]
        done;
        loop (i + 1) t
    | [] -> ()
  in
  loop 0 lines;
  matrix

let count_adj i j matrix : int =
  if matrix.(i).(j) <> '@' then 12
  else
    let i_max = Array.length matrix in
    let j_max = Array.length matrix.(0) in
    let is_roll (i, j) =
      if i < 0 || j < 0 || i >= i_max || j >= j_max then false
      else matrix.(i).(j) = '@'
    in
    [
      (i - 1, j);
      (i, j - 1);
      (i - 1, j - 1);
      (i + 1, j);
      (i, j + 1);
      (i + 1, j + 1);
      (i - 1, j + 1);
      (i + 1, j - 1);
    ]
    |> List.map is_roll
    |> List.map Bool.to_int
    |> List.fold_left ( + ) 0

let mapi_matrix fn matrix =
  Array.mapi (fun i row -> Array.mapi (fun j x -> fn i j matrix) row) matrix

let map_matrix fn matrix = Array.map (fun row -> Array.map fn row) matrix

let fold_left_matrix fn acc matrix =
  Array.fold_left (fun a row -> Array.fold_left fn a row) acc matrix

let process matrix =
  mapi_matrix count_adj matrix
  |> map_matrix (fun x -> if x < 4 then 1 else 0)
  |> fold_left_matrix ( + ) 0

let () =
  read_lines Sys.argv.(1) |> matrix_of_lines |> process |> Printf.printf "%i\n"
