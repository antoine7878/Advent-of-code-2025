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

let print_int_int x = Printf.printf "(%i,%i) " (fst x) (snd x)

let print_int_int_list lst =
  List.iter print_int_int lst;
  print_newline ()

let rec get_pos_to_remove = function
  | (h_i, h) :: (_, b) :: t when h < b -> h_i
  | [ (h_i, h) ] -> h_i
  | h :: t -> get_pos_to_remove t
  | [] -> raise (Failure "empty list")

let remove_n_min n lst : (int * int) list =
  let len = List.length lst in
  let rec loop i lst =
    if i < len - n then
      let pos = get_pos_to_remove lst in
      let lst = List.filter (fun x -> fst x != pos) lst in
      loop (i + 1) lst
    else lst
  in
  loop 0 lst

let () =
  let fold x acc = (acc * 10) + snd x in
  read_lines Sys.argv.(1)
  |> List.map list_of_string
  |> List.map (fun x -> List.mapi (fun i v -> (i, v)) x)
  |> List.map (fun x -> remove_n_min 12 x)
  |> List.map (fun z -> List.sort (fun x y -> -compare (fst x) (fst y)) z)
  |> List.map (fun z -> List.fold_right fold z 0)
  |> List.fold_left ( + ) 0
  |> Printf.printf "%i\n"
