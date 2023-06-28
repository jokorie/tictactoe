open! Core
open Tic_tac_toe_2023_common
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let avail_moves =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  if List.length avail_moves = 0
  then failwith "no more choices"
  else (
    let choice = List.random_element_exn avail_moves in
    choice)
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let w_moves =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  (* print_s [%message (w_moves : Position.t list)]; *)
  if List.length w_moves > 0
  then List.random_element_exn w_moves
  else random_move_strategy ~game_kind ~pieces
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_if_possible_strategy

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  ignore me;
  let score = Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces in
  match score with
  | Game_over { winner = Some X } -> Float.infinity
  | Game_over { winner = Some O } -> Float.neg_infinity
  | _ -> 0.0
;;

let rec minimax_helper
  ~(depth : int)
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  let all_moves =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  if depth = 0 || List.length all_moves = 0
  then score ~game_kind ~pieces ~me:Piece.X
  else (
    let board_evals =
      List.map
        ~f:(fun pos ->
          minimax_helper
            ~depth:(depth - 1)
            ~me:(Piece.flip me)
            ~game_kind
            ~pieces:(Map.set pieces ~key:pos ~data:me))
        all_moves
    in
    match me with
    | X ->
      (match List.max_elt ~compare:Float.compare board_evals with
       | Some num -> num
       | None -> score ~game_kind ~pieces ~me)
    | O ->
      (match List.min_elt ~compare:Float.compare board_evals with
       | Some num -> num
       | None -> score ~game_kind ~pieces ~me))
;;

(* (match me with | X -> List.map ~f:(fun pos -> minimax_helper ~depth:(depth
   -1) ~me:(Piece.flip me) ~game_kind ~pieces:(Map.set pieces pos me)
   all_moves) | O -> List.map ~f:(fun pos -> minimax_helper ~depth:(depth -1)
   ~me:(Piece.flip me) ~game_kind ~pieces:(Map.set pieces pos me)
   all_moves)) *)
(* (match List.max_elt all_moves ~compare:( minimax_helper ~depth:(depth - 1)
   ~me:(Piece.flip me) ~game_kind ~pieces:(Map.set pieces key value)) with
   Some number -> number | None -> 6.9) | O -> (match List.min_elt all_moves
   ~compare:(rec minimax_helper ~depth:(depth - 1) ~me:(Piece.flip me)
   ~game_kind ~pieces:(Map.set pieces key value)) with Some number -> number
   | None -> 6.9)) *)

let minimax
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t option
  =
  let all_moves =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  let _, best_pos =
    List.fold
      all_moves
      ~init:(0., None)
      ~f:(fun (curr_eval, curr_pos) next_pos ->
      let next_pos_eval = minimax_helper ~depth:6 ~me ~game_kind ~pieces in
      if Float.( >= ) next_pos_eval curr_eval
      then next_pos_eval, Some next_pos
      else curr_eval, curr_pos)
  in
  print_s [%message (best_pos : Position.t option)];
  best_pos
;;

let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let w_moves =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  (* print_s [%message (w_moves : Position.t list)]; *)
  if List.length w_moves > 0
  then (
    let move = List.random_element_exn w_moves in
    print_s [%message "winning" (move : Position.t)];
    move)
  else (
    let l_moves =
      Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces
    in
    if List.length l_moves > 0
    then (
      let move = List.random_element_exn l_moves in
      print_s [%message "blocking" (move : Position.t)];
      move)
    else (
      let best_move = minimax ~me ~game_kind ~pieces in
      match best_move with
      | Some pos ->
        print_s [%message "minimax" (pos : Position.t)];
        pos
      | None -> { Position.row = 6; column = 9 }))
;;

let _ = pick_winning_move_or_block_if_possible_strategy

(* List.max_elt all_moves ~compare:( minimax_helper ~depth:6 ~me:(Piece.flip
   me) ~game_kind ~pieces:(Map.set pieces key value)) | O -> List.min_elt
   all_moves ~compare:( minimax_helper ~depth:6 ~me:(Piece.flip me)
   ~game_kind ~pieces:(Map.set pieces key value)) );; *)

(* let rec evaluate_children ~depth:int ~me:(Piece.t) ~(game_kind :
   Game_kind.t) ~(pieces : Piece.t Position.Map.t) : float Position.t Tuple
   = *)

let _ = score

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  pick_winning_move_or_block_if_possible_strategy
    ~me
    ~game_kind:game_state.game_kind
    ~pieces:game_state.pieces
;;
