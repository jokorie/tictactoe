open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let down { Position.row; column } = { Position.row = row + 1; column }
let right { Position.row; column } = { Position.row; column = column + 1 }
let up { Position.row; column } = { Position.row = row - 1; column }
let _left { Position.row; column } = { Position.row; column = column - 1 }

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;

let win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let non_win =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

let all_slots ~(game_kind : Game_kind.t) =
  let length = Protocol.Game_kind.board_length game_kind in
  let all_coords =
    List.init length ~f:(fun x ->
      List.init length ~f:(fun y -> { Position.row = x; column = y }))
  in
  let concat_coords = List.concat all_coords in
  let set_all_coords = Position.Set.of_list concat_coords in
  set_all_coords
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let keys = Map.keys pieces in
  let set_of_keys = Position.Set.of_list keys in
  let all_slots = all_slots ~game_kind in
  let avail_pos = Set.diff all_slots set_of_keys in
  Set.to_list avail_pos
;;

let is_valid_coord ~(coord : Position.t) =
  (* coord.row >= 0 && coord.column >= 0 && coord.column <= 2 && coord.row <=
     2 *)
  Position.in_bounds coord ~game_kind:Game_kind.Tic_tac_toe
;;

let rec build_direction ~step_dir ~eval_dir ~(pos : Position.t) =
  if is_valid_coord ~coord:pos
  then [ pos ] @ build_direction ~step_dir ~eval_dir ~pos:(eval_dir pos)
  else []
;;

let rec generic_search_helper ~step_dir ~eval_dir ~(pos : Position.t)
  : Position.t list list
  =
  if is_valid_coord ~coord:pos
  then (
    let sequences =
      [ build_direction ~step_dir ~eval_dir ~pos ]
      @ generic_search_helper ~step_dir ~eval_dir ~pos:(step_dir pos)
    in
    sequences)
  else []
;;

let evaluate_seq
  ~(seq : Position.t list)
  ~(player : Piece.t)
  ~(pieces : Piece.t Position.Map.t)
  : bool
  =
  List.for_all seq ~f:(fun pos ->
    let piece = Map.find pieces pos in
    match piece with None -> false | Some p -> Piece.equal p player)
;;

let generic_search ~step_dir ~eval_dir ~(pos : Position.t) ~player ~pieces =
  List.exists
    (List.filter
       (generic_search_helper ~step_dir ~eval_dir ~pos)
       ~f:(fun list_seq -> List.length list_seq = 3))
    ~f:(fun seq -> evaluate_seq ~seq ~player ~pieces)
;;

let eval_rows ~player ~pieces =
  generic_search
    ~step_dir:down
    ~eval_dir:right
    ~pos:{ Position.row = 0; column = 0 }
    ~player
    ~pieces
;;

let eval_cols ~player ~pieces =
  generic_search
    ~step_dir:right
    ~eval_dir:down
    ~pos:{ Position.row = 0; column = 0 }
    ~player
    ~pieces
;;

let eval_diagonal ~player ~pieces =
  let tl_diagonal () =
    generic_search
      ~step_dir:down
      ~eval_dir:(fun x -> x |> up |> right)
      ~pos:{ Position.row = 0; column = 0 }
      ~player
      ~pieces
  in
  let br_diagonal () =
    generic_search
      ~step_dir:right
      ~eval_dir:(fun x -> x |> up |> right)
      ~pos:{ Position.row = 2; column = 0 }
      ~player
      ~pieces
  in
  let tr_diagonal () =
    generic_search
      ~step_dir:right
      ~eval_dir:(fun x -> x |> down |> right)
      ~pos:{ Position.row = 0; column = 0 }
      ~player
      ~pieces
  in
  let bl_diagonal () =
    generic_search
      ~step_dir:down
      ~eval_dir:(fun x -> x |> down |> right)
      ~pos:{ Position.row = 0; column = 0 }
      ~player
      ~pieces
  in
  let all_diagonals () =
    tl_diagonal () || br_diagonal () || tr_diagonal () || bl_diagonal ()
  in
  all_diagonals ()
;;

(* let find_cols ~(game_kind : Game_kind.t) : Position.t list list = ;; *)

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  ignore pieces;
  ignore game_kind;
  let x_won =
    eval_rows ~player:Piece.X ~pieces
    || eval_cols ~player:Piece.X ~pieces
    || eval_diagonal ~player:Piece.X ~pieces
  in
  let o_won =
    eval_rows ~player:Piece.O ~pieces
    || eval_cols ~player:Piece.O ~pieces
    || eval_diagonal ~player:Piece.O ~pieces
  in
  match x_won, o_won with
  | true, true -> Evaluation.Illegal_state
  | true, false -> Evaluation.Game_over { winner = Some X }
  | false, true -> Evaluation.Game_over { winner = Some O }
  | false, false ->
    if Map.length pieces
       < Game_kind.board_length game_kind * Game_kind.board_length game_kind
    then Evaluation.Game_continues
    else Evaluation.Game_over { winner = None }
;;

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let all_moves = available_moves ~game_kind ~pieces in
  let move_wins move =
    match
      evaluate ~game_kind ~pieces:(Map.set pieces ~key:move ~data:me)
    with
    | Game_over { winner = Some _ } -> true
    | _ -> false
  in
  List.filter all_moves ~f:move_wins
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let opp_piece = Piece.flip me in
  winning_moves ~me:opp_piece ~game_kind ~pieces
;;

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x.game_kind
           ~pieces:win_for_x.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         losing_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XOX
    OOX
    OXX |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

(* After you've implemented [available_moves], uncomment these tests! *)
(* let%expect_test "yes available_moves" = let (moves : Position.t list) =
   available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces |>
   List.sort ~compare:Position.compare in print_s [%sexp (moves : Position.t
   list)]; [%expect {| (((row 0) (column 1)) ((row 0) (column 2)) ((row 1)
   (column 1)) ((row 1) (column 2)) ((row 2) (column 1))) |}] ;;

   let%expect_test "no available_moves" = let (moves : Position.t list) =
   available_moves ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces |>
   List.sort ~compare:Position.compare in print_s [%sexp (moves : Position.t
   list)]; [%expect {| () |}] ;; *)

(* When you've implemented the [evaluate] function, uncomment the next two
   tests! *)
(* let%expect_test "evalulate_win_for_x" = (* print_endline
   (Game_state.to_string_hum win_for_x); *) print_endline (evaluate
   ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces |>
   Evaluation.to_string); [%expect {| (Game_over(winner(X))) |}] ;;

   let%expect_test "evalulate_non_win" = print_endline (evaluate
   ~game_kind:non_win.game_kind ~pieces:non_win.pieces |>
   Evaluation.to_string); [%expect {| Game_continues |}] ;;

   (* When you've implemented the [winning_moves] function, uncomment this
   test! *) let%expect_test "winning_move" = let positions = winning_moves
   ~game_kind:non_win.game_kind ~pieces:non_win.pieces ~me:Piece.X in print_s
   [%sexp (positions : Position.t list)]; [%expect {| ((((row 1) (column
   1)))) |}]; let positions = winning_moves ~game_kind:non_win.game_kind
   ~pieces:non_win.pieces ~me:Piece.O in print_s [%sexp (positions :
   Position.t list)]; [%expect {| () |}] ;; *)

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
let%expect_test "print_losing" =
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in

  print_s [%sexp (positions : Position.t list)];
  [%expect
    {|
  (((row 1) (column 1))) |}]
;;
