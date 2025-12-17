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

let process_line (beams : int array) (line : string) =
  String.iteri
    (fun i a ->
      if a = '^' then (
        beams.(i - 1) <- beams.(i - 1) + beams.(i);
        beams.(i + 1) <- beams.(i + 1) + beams.(i);
        beams.(i) <- 0))
    line

let init_beams line =
  Array.init (String.length line) (fun i -> if line.[i] = '.' then 0 else 1)

let print_beams beams =
  Array.iter (fun x -> if x = 0 then print_char '.' else print_int x) beams;
  print_newline ()

let process lines =
  let beams = init_beams (List.hd lines) in
  let rec loop lines =
    match lines with
    | h :: t ->
        process_line beams h;
        loop t
    | _ -> ()
  in
  loop (List.tl lines);
  beams

let () =
  read_lines Sys.argv.(1)
  |> process
  |> Array.fold_left ( + ) 0
  |> Printf.printf "res %i\n"
