let ( + ) = Int64.add
let ( - ) = Int64.sub
(* let ( * ) = Int64.mul *)
(* let ( / ) = Int64.div *)
(* let ( mod ) = Int64.rem *)

type interval = Int64.t * Int64.t

let print_interval (lhs, rhs) = Printf.printf "[%Li-%Li]\n" lhs rhs
let print_intervals intervals = List.iter print_interval intervals

let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

let parse_lines lines : interval list =
  let parse_interval line =
    String.split_on_char '-' line |> List.map Int64.of_string |> function
    | [ h; b ] when h <= b -> (h, b)
    | [ h; b ] -> failwith "inverted interval"
    | _ -> failwith "too many values splitting interval"
  in
  let rec loop lines =
    match lines with
    | h :: t when h = "" -> []
    | h :: t -> parse_interval h :: loop t
    | _ -> failwith "could not find empty separation line"
  in
  loop lines

let rec merge_intervals intervals =
  match intervals with
  | h :: b :: t ->
      if snd h >= fst b then
        merge_intervals ((fst h, Int64.max (snd h) (snd b)) :: t)
      else h :: merge_intervals (b :: t)
  | l -> l

let len_of_interval interval = snd interval - fst interval + 1L

let () =
  read_lines Sys.argv.(1)
  |> parse_lines
  |> List.sort compare
  |> merge_intervals
  |> List.fold_left (fun acc x -> acc + len_of_interval x) 0L
  |> Printf.printf "(%Li)\n"
