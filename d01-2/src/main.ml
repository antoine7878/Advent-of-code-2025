module Direction = struct
  type t = R | L

  let to_int = function
    | R -> 1
    | L -> -1
end

module Rotation = struct
  type t = { direction : Direction.t; count : int }

  let direction_of_char = function
    | 'R' -> Direction.R
    | 'L' -> Direction.L
    | _ -> raise (Failure "bad direction")

  let of_line (line : string) : t =
    {
      direction = direction_of_char line.[0];
      count = String.sub line 1 (String.length line - 1) |> int_of_string;
    }
end

module Dial = struct
  type t = { value : int; code : int }

  let print dial = Printf.printf "value: %i, code: %i\n" dial.value dial.code
  let dial value code = { value; code }

  let rotate (dial : t) (rotation : Rotation.t) : t =
    let modulo a b = if a >= 0 then a mod b else ((a mod b) + b) mod b in
    let new_val =
      modulo
        (dial.value + (Direction.to_int rotation.direction * rotation.count))
        100
    in
    { value = new_val; code = (dial.code + if new_val = 0 then 1 else 0) }
end

let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'

let () =
  read_lines Sys.argv.(1)
  |> List.map Rotation.of_line
  |> List.fold_left Dial.rotate (Dial.dial 50 0)
  |> Dial.print
