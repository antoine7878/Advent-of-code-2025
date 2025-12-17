let ( + ) = Int64.add
let ( - ) = Int64.sub
let ( * ) = Int64.mul
let ( / ) = Int64.div
let ( = ) = Int64.equal
let ( mod ) = Int64.rem

let read_intervals file : (Int64.t * Int64.t) list =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char ','
  |> List.map (String.split_on_char '-')
  |> List.map (function
    | [ rhs; lhs ] -> (Int64.of_string rhs, Int64.of_string lhs)
    | _ -> raise (Failure "bad interval"))

let rec count_digit x = if x < 10L then 1L else 1L + count_digit (x / 10L)

let rec pow a = function
  | 0L -> 1L
  | 1L -> a
  | n ->
      let b = pow a (n / 2L) in
      b * b * if n mod 2L = 0L then 1L else a

let split_code x =
  let count = count_digit x in
  let divi = pow 10L (count / 2L) in
  let lhs = x / divi in
  let rhs = x mod divi in
  (lhs, rhs, count, divi)

let get_next_invalid_code x =
  if x <= 10L then 11L
  else
    let lhs, rhs, count, divi = split_code x in
    if count mod 2L = 1L then (divi * divi * 10L) + divi
    else if lhs <= rhs then
      let lhs = lhs + 1L in
      (lhs * if lhs = divi then divi * 10L else divi) + lhs
    else (lhs * divi) + lhs

let is_invalid_code x : bool =
  let lhs, rhs, count, _ = split_code x in
  if count mod 2L = 1L then false
  else
    let lhs, rhs, _, _ = split_code x in
    rhs = lhs

let iter_interval interval : Int64.t =
  let lhs, rhs = interval in
  let rec loop_fold x =
    let next = get_next_invalid_code x in
    if next > rhs then 0L else next + loop_fold next
  in
  (if is_invalid_code lhs then lhs else 0L) + loop_fold lhs

let () =
  read_intervals Sys.argv.(1)
  |> List.map iter_interval
  |> List.fold_left ( + ) 0L
  |> Printf.printf "%Li\n%!"
