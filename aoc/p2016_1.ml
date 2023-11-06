open Core
open Helper

type point = {x: int; y: int}

let taxicab_distance p1 p2 = Int.abs (p1.x - p2.x) + Int.abs (p1.y - p2.y)

let%test _ = taxicab_distance {x= 0; y= 0} {x= 1; y= 1} = 2

type direction = Left | Right

type position = {orientation: int; point: point}

type instruction = {direction: direction; blocks: int}

let turn position direction =
  { position with
    orientation=
      (position.orientation + match direction with Left -> -90 | Right -> 90)
      % 360 }

let move_blocks position blocks =
  let delta_xy =
    match position.orientation with
    | 0 ->
        {x= 0; y= 1}
    | 90 ->
        {x= 1; y= 0}
    | 180 ->
        {x= 0; y= -1}
    | 270 ->
        {x= -1; y= 0}
    | _ ->
        raise (Failure "should not happen")
  in
  { position with
    point=
      { x= position.point.x + (delta_xy.x * blocks)
      ; y= position.point.y + (delta_xy.y * blocks) } }

let follow_instruction position instruction =
  let position = turn position instruction.direction in
  move_blocks position instruction.blocks

let rec follow_instructions ?(pos = {orientation= 0; point= {x= 0; y= 0}})
    instructions =
  match Sequence.next instructions with
  | None ->
      pos
  | Some (instruction, instructions) ->
      follow_instructions ~pos:(follow_instruction pos instruction) instructions

let blocks_away instructions =
  taxicab_distance {x= 0; y= 0} (follow_instructions instructions).point

let rec parse_int ?(int = 0) input =
  match Sequence.next input with
  | None ->
      (int, Sequence.empty)
  | Some (ch, input) ->
      if Char.is_digit ch then
        parse_int ~int:((int * 10) + (Char.to_int ch - Char.to_int '0')) input
      else (int, Sequence.append (Sequence.singleton ch) input)

let parse input =
  Sequence.unfold_step ~init:(Sequence.next input) ~f:(fun s ->
      match s with
      | None ->
          Sequence.Step.Done
      | Some ((('L' | 'R') as l_or_r), input) ->
          let i, input = parse_int input in
          Sequence.Step.Yield
            { value=
                { direction=
                    ( match l_or_r with
                    | 'L' ->
                        Left
                    | 'R' ->
                        Right
                    | _ ->
                        raise (Failure "unreachable") )
                ; blocks= i }
            ; state= Sequence.next input }
      | Some ((' ' | ',' | '\n'), input) ->
          Sequence.Step.Skip {state= Sequence.next input}
      | Some (ch, _) ->
          raise (Failure (Printf.sprintf "unexpected character %c" ch)) )

let%test "parse" =
  Poly.equal
    (Sequence.to_list (parse (string_to_sequence "L3, R4")))
    [{direction= Left; blocks= 3}; {direction= Right; blocks= 4}]

let blocks_away_string s = s |> string_to_sequence |> parse |> blocks_away

let%test _ = blocks_away_string "R2, L3" = 5

let%test _ = blocks_away_string "R2, R2, R2" = 2

let%test _ = blocks_away_string "R5, L5, R5, R3" = 12

let blocks_away_file f =
  In_channel.with_file f ~f:(fun ic ->
      ic |> channel_to_sequence |> parse |> blocks_away )

let%expect_test _ =
  Printf.printf "%d" (blocks_away_file "2016_1.txt") ;
  [%expect {| 299 |}]
