let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

module Machine = struct
  type t = { switches : int array list; joltage : int array }

  let switch_of_string len_joltages str =
    String.split_on_char ',' str |> List.map int_of_string |> fun x ->
    Array.init len_joltages (fun i -> if List.mem i x then 1 else 0)

  let joltage_of_string str =
    String.split_on_char ',' str |> List.map int_of_string |> Array.of_list

  let machine_of_line line =
    let trim str = String.sub str 1 (String.length str - 2) in
    let split_middle lst =
      match lst with
      | [] -> invalid_arg "empty list"
      | [ x ] -> (x, [], x)
      | first :: t ->
          let rec loop acc = function
            | [ last ] -> (first, List.rev acc, last)
            | x :: xs -> loop (x :: acc) xs
            | [] -> assert false
          in
          loop [] t
    in
    let _, switches_srts, joltage_str =
      String.split_on_char ' ' line |> List.map trim |> split_middle
    in
    let joltage = joltage_of_string joltage_str in
    let switches =
      List.map (switch_of_string (Array.length joltage)) switches_srts
    in
    { switches; joltage }
end

let problem (machine : Machine.t) =
  let open Lp in
  let ci x = c (float_of_int x) in
  let variables =
    List.mapi
      (fun i _ -> var ~integer:true ("b_" ^ string_of_int i))
      machine.switches
  in
  let sw_var = List.map2 (fun a b -> (a, b)) machine.switches variables in
  let constrains =
    Array.mapi
      (fun i t ->
        List.fold_left
          (fun acc (sw_array, v) -> acc ++ (ci sw_array.(i) *~ v))
          (ci 0) sw_var
        =~ ci t)
      machine.joltage
  in
  let objective =
    minimize (List.fold_left (fun acc x -> acc ++ x) (c 0.) variables)
  in
  make objective (Array.to_list constrains)

let solve machine =
  match Lp_highs.solve ~msg:false (problem machine) with
  | Ok (obj, _) -> int_of_float obj
  | Error msg -> failwith msg

let () =
  read_lines Sys.argv.(1)
  |> List.map Machine.machine_of_line
  |> List.map solve
  |> List.fold_left ( + ) 0
  |> Printf.printf "res: %i\n"
