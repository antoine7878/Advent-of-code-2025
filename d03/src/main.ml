type bank = int list

let list_of_string (s : string) : int list =
  List.init (String.length s) (String.get s)
  |> List.map (fun x -> int_of_char x - 48)

let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

let print_int_list lst =
  List.iter print_int lst;
  print_newline ()

let max_int_list_from (pos_from : int) (pos_to : int) (lst : 'a list) =
  lst
  |> List.mapi (fun i x -> (i, x))
  |> List.fold_left
       (fun acc x ->
         if fst x >= pos_from && fst x < pos_to && snd x > snd acc then x
         else acc)
       (-1, -1)

let get_two_max lst =
  let len = List.length lst in
  let first_i, first = max_int_list_from 0 len lst in
  if first_i = len - 1 then
    (snd (max_int_list_from 0 (len - 1) lst) * 10) + first
  else (first * 10) + snd (max_int_list_from (first_i + 1) len lst)

let () =
  read_lines Sys.argv.(1)
  |> List.map list_of_string
  |> List.map get_two_max
  |> List.fold_left ( + ) 0
  |> Printf.printf "%i\n"
