type matrix = char array array

let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

let print_matrix matrix =
  let print_array vec =
    Array.iter print_char vec;
    print_newline ()
  in
  Array.iter print_array matrix

let matrix_of_input lines : matrix =
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

let mark_to_remove i j matrix : char =
  if matrix.(i).(j) = '.' then '.'
  else
    let i_max = Array.length matrix in
    let j_max = Array.length matrix.(0) in
    let is_roll (i, j) =
      if i < 0 || j < 0 || i >= i_max || j >= j_max then false
      else matrix.(i).(j) = '@'
    in
    let count =
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
    in
    if count < 4 then 'x' else '@'

let mapi_matrix fn (matrix : matrix) : matrix =
  Array.mapi (fun i row -> Array.mapi (fun j x -> fn i j matrix) row) matrix

let map_matrix fn (matrix : matrix) : matrix =
  Array.map (fun row -> Array.map fn row) matrix

let fold_left_matrix fn acc (matrix : matrix) =
  Array.fold_left (fun a row -> Array.fold_left fn a row) acc matrix

let count_x (matrix : matrix) : int =
  Array.fold_left
    (fun acc row ->
      Array.fold_left (fun acc x -> acc + if x = 'x' then 1 else 0) acc row)
    0 matrix

let remove_x i j matrix = if matrix.(i).(j) = 'x' then '.' else matrix.(i).(j)

let process matrix =
  let turn (matrix : matrix) : int * matrix =
    mapi_matrix mark_to_remove matrix |> fun matrix ->
    ( fold_left_matrix (fun acc x -> acc + if x = 'x' then 1 else 0) 0 matrix,
      mapi_matrix remove_x matrix )
  in
  let rec loop matrix =
    match turn matrix with
    | count, _ when count = 0 -> 0
    | count, new_matrix -> count + loop new_matrix
  in
  loop matrix

let () =
  read_lines Sys.argv.(1) |> matrix_of_input |> process |> Printf.printf "%i\n"
