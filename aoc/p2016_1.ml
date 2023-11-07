open Core
open Helper

let taxicab_distance p1 p2 = abs (p1.x - p2.x) + abs (p1.y - p2.y)

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
    ?(visited_points = None) instructions =
  if
    Option.value_map visited_points
      ~f:(fun visited_points -> Set.mem visited_points pos.point)
      ~default:false
  then pos
  else
    match Sequence.next instructions with
    | None ->
        pos
    | Some (instruction, instructions) ->
        let pos' = follow_instruction pos instruction in
        follow_instructions ~pos:pos'
          ~visited_points:
            (Option.map visited_points ~f:(fun s -> Set.add s pos.point))
          instructions

let blocks_away ?(visited_points = None) instructions =
  taxicab_distance {x= 0; y= 0}
    (follow_instructions instructions ~visited_points).point

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

let blocks_away_string ?(visited_points = None) s =
  s |> string_to_sequence |> parse |> blocks_away ~visited_points

let%test _ = blocks_away_string "R2, L3" = 5

let%test _ = blocks_away_string "R2, R2, R2" = 2

let%test _ = blocks_away_string "R5, L5, R5, R3" = 12

let blocks_away_file ?(visited_points = None) f =
  In_channel.with_file f ~f:(fun ic ->
      ic |> channel_to_sequence |> parse |> blocks_away ~visited_points )

let%expect_test _ =
  Printf.printf "%d" (blocks_away_file "2016_1.txt") ;
  [%expect {| 299 |}]

let encountered_twice instructions =
  follow_instructions
    ~visited_points:(Some (Set.empty (module PointAscending)))
    instructions

let encountered_twice_string s =
  s |> string_to_sequence |> parse |> encountered_twice

let encountered_twice_file f =
  In_channel.with_file f ~f:(fun ic ->
      ic |> channel_to_sequence |> parse |> encountered_twice )

let%expect_test _ =
  Printf.printf "%d"
    (blocks_away_string "R8, R4, R4, R8"
       ~visited_points:(Some (Set.empty (module PointAscending))) ) ;
  [%expect {| 4 |}]

let%expect_test _ =
  Printf.printf "%d"
    (blocks_away_file "2016_1.txt"
       ~visited_points:(Some (Set.empty (module PointAscending))) ) ;
  [%expect {| 299 |}]
