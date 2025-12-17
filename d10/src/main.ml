let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

module Machine = struct
  type t = { lights : int; switches : int list; max_light : int }

  let lights_of_string str =
    String.fold_right
      (fun x acc ->
        let acc = acc lsl 1 in
        if x = '#' then acc + 1 else acc)
      str 0

  let switch_of_str str =
    String.split_on_char ',' str
    |> List.map int_of_string
    |> List.map (( lsl ) 1)
    |> List.fold_left ( + ) 0

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
    let light_str, switches_srts, _ =
      String.split_on_char ' ' line |> List.map trim |> split_middle
    in
    let lights = lights_of_string light_str in
    let max_light =
      String.fold_left (fun acc _ -> (acc + 1) lsl 1) 0 light_str
    in
    let switches = List.map switch_of_str switches_srts in
    { lights; switches; max_light }
end

let solve (machine : Machine.t) =
  let state_queue = Queue.create () in
  Queue.push (0, 0) state_queue;
  let visited = Array.make machine.max_light true in

  let rec loop () =
    let steps, current_state = Queue.pop state_queue in
    let rec check_switches = function
      | [] -> None
      | h :: t ->
          let new_state = current_state lxor h in
          if new_state = machine.lights then Some (steps + 1)
          else (
            if visited.(new_state) then (
              visited.(new_state) <- false;
              Queue.push (steps + 1, new_state) state_queue);
            check_switches t)
    in
    match check_switches machine.switches with
    | Some res -> res
    | None -> loop ()
  in
  loop ()

let () =
  read_lines Sys.argv.(1)
  |> List.map Machine.machine_of_line
  |> List.map solve
  |> List.fold_left ( + ) 0
  |> Printf.printf "res: %i\n"
