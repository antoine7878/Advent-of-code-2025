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
  |> List.map (String.split_on_char ' ')
  |> List.map (List.filter (( <> ) ""))

let rec split_last_line lines =
  match List.rev lines with
  | h :: t -> (List.map (List.map Int64.of_string) t, h)
  | _ -> failwith "bad input"

let rec transpose lst =
  match lst with
  | [] -> []
  | [] :: t -> transpose t
  | (h :: t) :: tt ->
      (h :: List.map List.hd tt) :: transpose (t :: List.map List.tl tt)

let process ((lines, operations) : Int64.t list list * string list) =
  let lines = transpose lines in
  List.map2
    (fun line op ->
      List.fold_left
        (operation_of_str_operator op)
        (int_of_str_operator op) line)
    lines operations

let () = read_lines Sys.argv.(1) |> split_last_line |> process |> ignore
(* |> List.fold_left ( + ) 0L *)
(* |> Printf.printf "res: %Li\n" *)
