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

let print_list lst =
  List.iter (Printf.printf "%Li-") lst;
  print_newline ()

let rec pow a = function
  | 0L -> 1L
  | 1L -> a
  | n ->
      let b = pow a (n / 2L) in
      b * b * if n mod 2L = 0L then 1L else a

let split_chunks chunks_size x =
  let m = pow 10L chunks_size in
  let rec loop x =
    match x with
    | x when x < m -> [ x ]
    | x -> (x mod m) :: loop (x / m)
  in
  loop x

let is_invalid_code x : bool =
  let count = count_digit x in
  let rec loop n =
    (* Printf.printf "%Li,%Li,%Li,%Li\n" x count n (count mod n); *)
    match n with
    | n when n > count / 2L -> false
    | n when count mod n <> 0L -> loop (n + 1L)
    | n -> (
        match split_chunks n x with
        | [ h ] -> false
        | h :: t -> List.for_all (fun x -> x = h) t || loop (n + 1L)
        | _ -> false)
  in
  let res = loop 1L in
  res

let iter_interval interval : Int64.t =
  let lhs, rhs = interval in
  let rec loop = function
    | x when x > rhs -> 0L
    | x when is_invalid_code x -> x + loop (x + 1L)
    | x -> loop (x + 1L)
  in
  loop lhs

let () =
  read_intervals Sys.argv.(1)
  |> List.map iter_interval
  |> List.fold_left ( + ) 0L
  |> Printf.printf "%Li\n%!"
(* quand le vehicule se deplace ver une plance in cour de confimation test que  les recherches et reservations sont tjrs possible *)
