let ( + ) = Int64.add
let ( * ) = Int64.mul

let int_of_str_operator = function
  | "+" -> 0L
  | "*" -> 1L
  | _ -> failwith "bad input"

let operation_of_str_operator = function
  | "+" -> ( + )
  | "*" -> ( * )
  | _ -> failwith "bad input"

let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

let rec transpose_str_lst (lst : string list) : string list =
  let hd_of_string str =
    if String.length str < 1 then "" else String.sub str 0 1
  in
  let tl_of_string str =
    if String.length str < 1 then ""
    else String.sub str 1 (String.length str - 1)
  in
  match lst with
  | [] -> []
  | "" :: t -> transpose_str_lst t
  | str :: tt ->
      let h, t = (hd_of_string str, tl_of_string str) in
      (h ^ String.concat "" (List.map hd_of_string tt))
      :: transpose_str_lst (t :: List.map tl_of_string tt)

let merge_list (lst : string list list) =
  let rec loop lst =
    match lst with
    | h :: b :: t when (( = ) "") (List.hd b) -> h :: loop t
    | h :: b :: t -> loop ((h @ b) :: t)
    | [ h ] -> [ h ]
    | [] -> []
  in
  loop lst

let string_rev str =
  let len = String.length str in
  String.init len (fun i -> str.[len - i - 1])

let process lines =
  match List.rev lines with
  | h :: t ->
      let nums =
        transpose_str_lst t
        |> List.map String.trim
        |> List.map string_rev
        |> List.map List.singleton
        |> merge_list
        |> List.map (List.map Int64.of_string)
      in
      let ops = String.split_on_char ' ' h |> List.filter (( <> ) "") in
      (nums, ops)
  | _ -> failwith "bad input"

let calc ((lines, operations) : Int64.t list list * string list) =
  List.map2
    (fun line op ->
      List.fold_left
        (operation_of_str_operator op)
        (int_of_str_operator op) line)
    lines operations

let () =
  read_lines Sys.argv.(1)
  |> process
  |> calc
  |> List.fold_left ( + ) 0L
  |> Printf.printf "res: %Li\n"
