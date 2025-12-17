let ( + ) = Int64.add
(* let ( - ) = Int64.sub *)
(* let ( * ) = Int64.mul *)
(* let ( / ) = Int64.div *)
(* let ( mod ) = Int64.rem *)

type interval = Int64.t * Int64.t

let print_interval (lhs, rhs) = Printf.printf "[%Li-%Li]\n" lhs rhs
let print_intervals intervals = List.iter print_interval intervals

let parse_interval line =
  String.split_on_char '-' line |> List.map Int64.of_string |> function
  | [ h; b ] when h <= b -> (h, b)
  | [ h; b ] -> raise (Failure "inverted interval")
  | _ -> raise (Failure "too many values splitting interval")

let merge_window intervals intervals_next = ()

let rec merge_intervals intervals =
  match intervals with
  | h :: b :: t ->
      if snd h >= fst b then
        merge_intervals ((fst h, Int64.max (snd h) (snd b)) :: t)
      else h :: merge_intervals (b :: t)
  | l -> l

type id = Int64.t

let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

let parse_lines lines : interval list * id list =
  let rec parse_ids lines : id list =
    match lines with
    | h :: t -> Int64.of_string h :: parse_ids t
    | [] -> []
  in
  let rec parse_intervals lines : interval list * string list =
    match lines with
    | h :: t when h = "" -> ([], t)
    | h :: t ->
        let next_intervals, new_tail = parse_intervals t in
        (parse_interval h :: next_intervals, new_tail)
    | _ -> raise (Failure "could not find empty separation line")
  in
  let intervals, line = parse_intervals lines in
  let ids = parse_ids line in
  (intervals, ids)

let prepare_data (intervals, ids) =
  (List.sort compare intervals |> merge_intervals, List.sort compare ids)

let count_valid_ids (intervals, ids) =
  let rec loop intervals ids =
    match (intervals, ids) with
    | [], _ -> 0L
    | _, [] -> 0L
    | inter :: t_inter, id :: t_id when id < fst inter ->
        loop (inter :: t_inter) t_id
    | inter :: t_inter, id :: t_id when id > snd inter ->
        loop t_inter (id :: t_id)
    | inter :: t_inter, id :: t_id -> 1L + loop (inter :: t_inter) t_id
  in
  loop intervals ids

let () =
  read_lines Sys.argv.(1)
  |> parse_lines
  |> prepare_data
  |> count_valid_ids
  |> Printf.printf "(%Li)\n"
