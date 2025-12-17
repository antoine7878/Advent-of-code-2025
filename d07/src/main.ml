let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'
  |> List.filteri (fun i _ -> i mod 2 = 0)

let remove_duplicate cmp lst =
  let rec loop = function
    | h :: b :: t when cmp h b = 0 -> loop (h :: t)
    | h :: t -> h :: loop t
    | [] -> []
  in
  loop lst

let process_line (beams : int list) (line : string) =
  let split_beam beam =
    match line.[beam] with
    | '^' -> [ beam - 1; beam + 1 ]
    | _ -> [ beam ]
  in
  let split_count =
    List.fold_left
      (fun acc beam -> acc + if line.[beam] = '^' then 1 else 0)
      0 beams
  in
  let new_beams =
    List.map split_beam beams |> List.flatten |> remove_duplicate compare
  in
  (split_count, new_beams)

let process lines =
  let beams = [ String.index_from (List.hd lines) 0 'S' ] in
  List.fold_left
    (fun (acc_count, beams) line ->
      let split_count, beams = process_line beams line in
      (split_count + acc_count, beams))
    (0, beams) lines

let () =
  let count, beams = read_lines Sys.argv.(1) |> process in
  Printf.printf "res: %i\n" count
