(* Testing summary. We employed both blackbox and glassbox testing. We also did
   both modular and unit testing. Blackbox tests were constructed for functions
   that implemented chess logic. These tests were based on how the game of chess
   works rather than the implementation of the functions. An example for this is
   that when testing update_location, which moves a piece and updates the
   board's and the piece's information, we check the old location is empty, the
   new location has the piece we moved, and the location of the piece is the new
   location. Then modular tests were made to make sure that each component of
   the chess game was working as intended. Unit tests were developed as
   functions were implemented to ensure that they worked correctly. We also used
   bisect to make sure that our code reached high coverage. Our game was fully
   manually tested. There was no use of QCheck or randomized testing. Our tests
   show correctness of the program because our tests were construcuted to test
   how our game's behavior aligns with a real chess game. We check the game is
   initialized correctly, all possible moves that a piece can make, and
   checkmates.*)

open OUnit2
open Game
open Pieces
open Board
open Logic
(*****************************************************************)
(* Test suite *)
(*****************************************************************)

(** [valid_move_test name] constructs an OUnit test in [board_tests] that
    asserts the quality of [expected_output] with [valid_move board move color]. *)
let valid_move_test (name : string) (board : board) (move : int * int)
    (color : zcolor) (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (valid_move board move color)

let tp_piece board (r, c) piece : board =
  let removed =
    List.filter
      (fun a ->
        match a with
        | Full (r', c', _) | Empty (r', c') ->
            if r = r' && c = c' then false else true)
      board
  in
  Full (r, c, piece) :: removed

let rec print_helper = function
  | [] -> print_string ""
  | u1 :: u2 ->
      print_string (u1 ^ " ");
      print_helper u2

let rec print_board = function
  | [] -> print_endline ""
  | u1 :: u2 ->
      print_endline "";
      print_helper u1;
      print_board u2

(** [update_board_test name] constructs an OUnit test in [board_tests] that
    asserts the quality of [expected_output] with [update_board move piece]. .*)
let update_board_test (name : string) (board : board) (move : int * int)
    (piece : Pieces.t) : test =
  let updated_board = update_board board move piece in
  let old_loc = location piece in

  name >:: fun _ ->
  assert_equal (is_empty updated_board move) false;
  assert_equal (is_empty updated_board old_loc) true;
  assert_equal
    (find_piece move updated_board)
    (Pieces.update_location piece move)

(** [find_piece_test name] constructs an OUnit test in [board_tests] that
    asserts the quality of [expected_output] with [find_piece coord board]. *)
let find_piece_test (name : string) (coord : int * int) (board : board)
    (expected_output : Pieces.t) : test =
  name >:: fun _ -> assert_equal expected_output (find_piece coord board)

(** [is_empty_test name] constructs an OUnit test in [board_tests] that asserts
    the quality of [expected_output] with [is_empty board coord]. *)
let is_empty_test (name : string) (board : board) (coord : int * int)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (is_empty board coord)

let check_mate_two_rooks : board =
  let res = ref [] in
  empty_board res;
  (* White king at D1*)
  let k1 = tp_piece !res (1, 4) (Pieces.init King White (1, 4)) in
  (* Black rook at A2*)
  let r1 = tp_piece k1 (2, 1) (Pieces.init Rook Black (2, 1)) in
  (* Black King at F5 *)
  let k2 = tp_piece r1 (5, 6) (Pieces.init King Black (5, 6)) in
  (* 2nd Black Rook rook at A1. CHECKMATE *)
  tp_piece k2 (1, 1) (Pieces.init Rook Black (1, 1))

let check_mate_queen_king : board =
  let res = ref [] in
  empty_board res;
  (*White queen at E8*)
  let wq = tp_piece !res (8, 5) (Pieces.init Queen White (8, 5)) in
  (*Black King at G8*)
  let bk = tp_piece wq (8, 7) (Pieces.init King Black (8, 7)) in
  (*White King at G6 CHECKMATE*)
  tp_piece bk (6, 7) (Pieces.init King White (6, 7))

let check_mate_rook_king : board =
  let res = ref [] in
  empty_board res;
  (*Black king at B8*)
  let bk = tp_piece !res (8, 2) (Pieces.init King Black (8, 2)) in
  (*White Rook at E8*)
  let wr = tp_piece bk (8, 5) (Pieces.init Rook White (8, 5)) in
  (*White King at B6 CHECKMATE*)
  tp_piece wr (6, 2) (Pieces.init King White (6, 2))

let fools_checkmate : board =
  let start = init in
  let white_pawn_1 =
    (*White pawn from F2 to F3*)
    update_board start (3, 6) (Pieces.init Pawn White (2, 6))
  in
  (*Black pawn from E7 to E5*)
  let black_pawn =
    update_board white_pawn_1 (5, 5) (Pieces.init Pawn Black (7, 5))
  in
  (*White pawn from G2 to G4*)
  let white_pawn_2 =
    update_board black_pawn (4, 7) (Pieces.init Pawn White (2, 7))
  in
  (*Black Queen from D8 to H4 CHECKMATE*)
  update_board white_pawn_2 (4, 8) (Pieces.init Queen Black (8, 4))

let knight_check_1 : board =
  let res = ref [] in
  empty_board res;
  let k1 = tp_piece !res (8, 4) (Pieces.init King Black (8, 4)) in
  let kn = tp_piece k1 (6, 3) (Pieces.init Knight White (6, 3)) in
  tp_piece kn (1, 4) (Pieces.init King White (1, 4))

let knight_check_2 : board =
  let res = ref [] in
  empty_board res;
  let k1 = tp_piece !res (8, 4) (Pieces.init King Black (7, 5)) in
  let kn = tp_piece k1 (6, 3) (Pieces.init Knight White (6, 3)) in
  tp_piece kn (1, 4) (Pieces.init King White (1, 4))

let knight_check_3 : board =
  let res = ref [] in
  empty_board res;
  let k1 = tp_piece !res (8, 4) (Pieces.init King Black (5, 5)) in
  let kn = tp_piece k1 (6, 3) (Pieces.init Knight White (6, 3)) in
  tp_piece kn (1, 4) (Pieces.init King White (1, 4))

let knight_check_4 : board =
  let res = ref [] in
  empty_board res;
  let k1 = tp_piece !res (8, 4) (Pieces.init King Black (4, 2)) in
  let kn = tp_piece k1 (6, 3) (Pieces.init Knight White (6, 3)) in
  tp_piece kn (1, 4) (Pieces.init King White (1, 4))

let knight_check_5 : board =
  let res = ref [] in
  empty_board res;
  let k1 = tp_piece !res (8, 4) (Pieces.init King Black (5, 1)) in
  let kn = tp_piece k1 (6, 3) (Pieces.init Knight White (6, 3)) in
  tp_piece kn (1, 4) (Pieces.init King White (1, 4))

let knight_check_6 : board =
  let res = ref [] in
  empty_board res;
  let k1 = tp_piece !res (8, 4) (Pieces.init King Black (7, 1)) in
  let kn = tp_piece k1 (6, 3) (Pieces.init Knight White (6, 3)) in
  tp_piece kn (1, 4) (Pieces.init King White (1, 4))

let knight_check_7 : board =
  let res = ref [] in
  empty_board res;
  let k1 = tp_piece !res (8, 4) (Pieces.init King Black (8, 2)) in
  let kn = tp_piece k1 (6, 3) (Pieces.init Knight White (6, 3)) in
  tp_piece kn (1, 4) (Pieces.init King White (1, 4))

let knight_check_8 : board =
  let res = ref [] in
  empty_board res;
  let k1 = tp_piece !res (8, 4) (Pieces.init King Black (4, 4)) in
  let kn = tp_piece k1 (6, 3) (Pieces.init Knight White (6, 3)) in
  tp_piece kn (1, 4) (Pieces.init King White (1, 4))

let rook_check_1 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (7, 4)) in
  tp_piece bk (5, 4) (Pieces.init Rook White (5, 4))

let rook_check_2 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (5, 7) (Pieces.init King Black (5, 7)) in
  tp_piece bk (5, 4) (Pieces.init Rook White (5, 4))

let rook_check_3 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (5, 1) (Pieces.init King Black (5, 1)) in
  tp_piece bk (5, 4) (Pieces.init Rook White (5, 4))

let rook_check_4 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (3, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (5, 4) (Pieces.init Rook White (5, 4))

let bishop_check_1 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (5, 6) (Pieces.init Bishop White (5, 6))

let bishop_check_2 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (1, 6) (Pieces.init Bishop White (1, 6))

let bishop_check_3 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (1, 2) (Pieces.init Bishop White (1, 2))

let bishop_check_4 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (5, 2) (Pieces.init Bishop White (5, 2))

let queen_check_1 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (5, 6) (Pieces.init Queen White (5, 6))

let queen_check_2 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (3, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (3, 6) (Pieces.init Queen White (3, 6))

let queen_check_3 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (1, 2) (Pieces.init Queen White (1, 2))

let queen_check_4 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (3, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (5, 2) (Pieces.init Queen White (5, 2))

let queen_check_5 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (7, 4)) in
  tp_piece bk (5, 4) (Pieces.init Queen White (5, 4))

let queen_check_6 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (5, 7) (Pieces.init King Black (5, 7)) in
  tp_piece bk (5, 4) (Pieces.init Queen White (5, 4))

let queen_check_7 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (5, 1) (Pieces.init King Black (5, 1)) in
  tp_piece bk (5, 4) (Pieces.init Queen White (5, 4))

let queen_check_8 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (3, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (5, 4) (Pieces.init Queen White (5, 4))

let pawn_check_1 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (5, 4) (Pieces.init Pawn White (2, 3))

let pawn_check_2 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (5, 4) (Pieces.init Pawn White (2, 5))

let pawn_check_3 : board =
  let res = ref [] in
  empty_board res;
  let wk = tp_piece !res (1, 1) (Pieces.init King White (1, 1)) in
  let bk = tp_piece wk (7, 4) (Pieces.init King Black (3, 4)) in
  tp_piece bk (5, 4) (Pieces.init Pawn White (2, 4))

let king_check_1 : board =
  let res = ref [] in
  empty_board res;
  let bk = tp_piece !res (3, 3) (Pieces.init King Black (3, 3)) in
  tp_piece bk (2, 3) (Pieces.init King White (2, 3))

let king_check_2 : board =
  let res = ref [] in
  empty_board res;
  let bk = tp_piece !res (3, 3) (Pieces.init King Black (3, 3)) in
  tp_piece bk (2, 2) (Pieces.init King White (2, 2))

let king_check_3 : board =
  let res = ref [] in
  empty_board res;
  let bk = tp_piece !res (3, 3) (Pieces.init King Black (3, 3)) in
  tp_piece bk (3, 2) (Pieces.init King White (3, 2))

let king_check_4 : board =
  let res = ref [] in
  empty_board res;
  let bk = tp_piece !res (3, 3) (Pieces.init King Black (3, 3)) in
  tp_piece bk (4, 2) (Pieces.init King White (4, 2))

let king_check_5 : board =
  let res = ref [] in
  empty_board res;
  let bk = tp_piece !res (3, 3) (Pieces.init King Black (3, 3)) in
  tp_piece bk (4, 3) (Pieces.init King White (4, 3))

let king_check_6 : board =
  let res = ref [] in
  empty_board res;
  let bk = tp_piece !res (3, 3) (Pieces.init King Black (3, 3)) in
  tp_piece bk (4, 4) (Pieces.init King White (4, 4))

let king_check_7 : board =
  let res = ref [] in
  empty_board res;
  let bk = tp_piece !res (3, 3) (Pieces.init King Black (3, 3)) in
  tp_piece bk (3, 4) (Pieces.init King White (3, 4))

let king_check_8 : board =
  let res = ref [] in
  empty_board res;
  let bk = tp_piece !res (3, 3) (Pieces.init King Black (3, 3)) in
  tp_piece bk (2, 4) (Pieces.init King White (2, 4))

let smothered_checkmate : board =
  let res = ref [] in
  empty_board res;
  let br = tp_piece !res (8, 7) (Pieces.init Rook Black (8, 7)) in
  let bk = tp_piece br (8, 8) (Pieces.init King Black (8, 8)) in
  let bp1 = tp_piece bk (7, 7) (Pieces.init Pawn Black (7, 7)) in
  let bp2 = tp_piece bp1 (7, 8) (Pieces.init Pawn Black (7, 8)) in
  let wk = tp_piece bp2 (1, 7) (Pieces.init King White (1, 7)) in
  tp_piece wk (7, 6) (Pieces.init Knight White (7, 6))

let invalid_move_into_valid_move : test =
  let res = ref [] in
  empty_board res;
  let bk = tp_piece !res (8, 5) (Pieces.init King Black (8, 5)) in
  let wk = tp_piece bk (1, 5) (Pieces.init King White (1, 5)) in
  let blocking_pawn = tp_piece wk (3, 1) (Pieces.init Pawn White (3, 1)) in
  let wr = tp_piece blocking_pawn (1, 1) (Pieces.init Rook White (1, 1)) in
  ignore (check_move (5, 1) (Pieces.init Rook White (1, 1)) wr White);
  "Moving valid after invalid move" >:: fun _ ->
  assert_equal (check_move (2, 1) (Pieces.init Rook White (1, 1)) wr White) true
(*****************************************************************)

(* * [check_move_test name] constructs an OUnit test in [logic_tests] that
   asserts the quality of [expected_output] with [check_move move info board
   color]. *)
let check_move_test (name : string) (move : int * int) (info : Pieces.t)
    (board : board) (color : Pieces.zcolor) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (check_move move info board color)

(** [check_test name] constructs an OUnit test in [logic_tests] that asserts the
    quality of [expected_output] with [check board color]. *)
let check_test (name : string) (board : board) (color : Pieces.zcolor)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (check board color)

(** [check_mate_test name] constructs an OUnit test in [logic_tests] that
    asserts the quality of [expected_output] with [check_mate board color]. *)
let check_mate_test (name : string) (board : board) (color : Pieces.zcolor)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (check_mate board color)

let rec pawn_army_check color =
  let res = ref [] in
  match color with
  | White ->
      loop_through_row 2 White res;
      !res
  | Black ->
      loop_through_row 7 Black res;
      !res

and loop_through_row row color tests =
  let i = ref 1 in
  let name =
    color_to_string color ^ " pawn should be at " ^ "(" ^ string_of_int row
    ^ "," ^ string_of_int !i ^ ")"
  in
  while !i < 9 do
    tests :=
      find_piece_test name (row, !i) init (Pieces.init Pawn color (row, !i))
      :: !tests;
    i := !i + 1
  done

let pawn_init_check = pawn_army_check White @ pawn_army_check Black

let board_tests =
  [
    valid_move_test "test for if a piece can move to a place" init (1, 1) White
      false;
    valid_move_test "an empty spot should be valid to move to as black" init
      (5, 5) Black true;
    valid_move_test "moving to an unoccupied space as white should be true" init
      (5, 5) White true;
    valid_move_test "cannot move to an occupied test as black" init (8, 8) Black
      false;
    (*Testing initalization of pieces besides pawn is correct*)
    (*White side*)
    find_piece_test "White rook 1 should be at (1,1)" (1, 1) init
      (Pieces.init Rook White (1, 1));
    find_piece_test "White knight 1 should be at (1,2)" (1, 2) init
      (Pieces.init Knight White (1, 2));
    find_piece_test "White bishop 1 should be at (1,3)" (1, 3) init
      (Pieces.init Bishop White (1, 3));
    find_piece_test "White Queen should be at (1,4)" (1, 4) init
      (Pieces.init Queen White (1, 4));
    find_piece_test "White King should be at (1,5)" (1, 5) init
      (Pieces.init King White (1, 5));
    find_piece_test "White bishop 2 should be at (1,6)" (1, 6) init
      (Pieces.init Bishop White (1, 6));
    find_piece_test "White knight 2 should be at (1,7)" (1, 7) init
      (Pieces.init Knight White (1, 7));
    find_piece_test "White rook 2 should be at (1,1)" (1, 8) init
      (Pieces.init Rook White (1, 8));
    (*Black side*)
    find_piece_test "Black rook should be at (8,1)" (8, 1) init
      (Pieces.init Rook Black (8, 1));
    find_piece_test "Black knight 1 should be at (8,2) " (8, 2) init
      (Pieces.init Knight Black (8, 2));
    find_piece_test "Black bishop 1 should be at (8,3) " (8, 3) init
      (Pieces.init Bishop Black (8, 3));
    find_piece_test "Black Queen should be at (8,4) " (8, 4) init
      (Pieces.init Queen Black (8, 4));
    find_piece_test "Black King should be at (8,5) " (8, 5) init
      (Pieces.init King Black (8, 5));
    find_piece_test "Black bishop 2 should be at (8,6) " (8, 6) init
      (Pieces.init Bishop Black (8, 6));
    find_piece_test "Black knight 2 should be at (8,7) " (8, 7) init
      (Pieces.init Knight Black (8, 7));
    find_piece_test "Black rook 2 should be at (8,8) " (8, 8) init
      (Pieces.init Rook Black (8, 8))
    (*-------------------------------------------------------*);
    is_empty_test "(1,1) is initally occupied" init (1, 1) false;
    update_board_test "moving pawn at (2,1) to (3,1)" init (3, 1)
      (Pieces.init Pawn White (2, 1));
    find_piece_test "White rook should be at (1,1)" (1, 1) init
      (Pieces.init Rook White (1, 1));
    find_piece_test "Black rook should be at (8,1)" (8, 1) init
      (Pieces.init Rook Black (8, 1));
    find_piece_test "White pawn should be at (2,1)" (2, 1) init
      (Pieces.init Pawn White (2, 1));
    find_piece_test "Black pawn should be at (7,1)" (7, 1) init
      (Pieces.init Pawn Black (7, 1));
  ]
  (*Check pawns are initialized at the right places*)
  @ pawn_init_check

let command_tests = []

let logic_tests =
  [
    check_move_test "white pawn cannot teleport to where rook is" (1, 1)
      (Pieces.init Pawn White (2, 1))
      init White false;
    check_move_test "black cannot move white pawn" (2, 2)
      (Pieces.init Pawn White (2, 1))
      init Black false;
    check_move_test "white pawn cannot move in place" (2, 1)
      (Pieces.init Pawn White (2, 1))
      init White false;
    check_move_test "white pawn can move 1 forward initially" (3, 1)
      (Pieces.init Pawn White (2, 1))
      init White true;
    check_move_test "white pawn can move 2 forward initially" (4, 1)
      (Pieces.init Pawn White (2, 1))
      init White true;
    check_move_test "white pawn cannot move 2 after moving already" (6, 1)
      (Pieces.init Pawn White (3, 1))
      init White false;
    check_move_test "white pawn cannot move diagonally if empty" (2, 2)
      (Pieces.init Pawn White (2, 1))
      init White false;
    (let capture_poss =
       update_board init (3, 3) (Pieces.init Pawn Black (7, 2))
     in
     check_move_test
       "white pawn can move diagonally right to capture an enemy piece" (3, 3)
       (Pieces.init Pawn White (2, 2))
       capture_poss White true);
    (let capture_poss =
       update_board init (3, 3) (Pieces.init Pawn White (7, 2))
     in
     check_move_test
       "white pawn cannot move diagonally to capture an ally piece" (3, 3)
       (Pieces.init Pawn White (2, 2))
       capture_poss White false);
    (let capture_poss =
       update_board init (3, 1) (Pieces.init Pawn Black (7, 2))
     in
     check_move_test
       "white pawn can move left diagonally to capture an enemy piece" (3, 1)
       (Pieces.init Pawn White (2, 2))
       capture_poss White true);
    check_move_test "white cannot move a black piece" (6, 1)
      (Pieces.init Pawn Black (7, 1))
      init White false;
    (let capture_poss =
       update_board init (6, 1) (Pieces.init Pawn White (2, 1))
     in
     check_move_test
       "black pawn can move left diagonally to capture an enemy piece" (6, 1)
       (Pieces.init Pawn Black (7, 2))
       capture_poss Black true);
    check_move_test "black pawn can move forward 2 spaces initally" (5, 1)
      (Pieces.init Pawn Black (7, 1))
      init Black true;
    check_move_test "black pawn cannot move 2 after moving already" (5, 1)
      (Pieces.init Pawn Black (3, 1))
      init Black false;
    check_move_test "black pawn can move forward 1" (6, 1)
      (Pieces.init Pawn Black (7, 1))
      init Black true;
    check_move_test "white knight can make a legal move from start" (3, 6)
      (Pieces.init Knight White (1, 7))
      init White true;
    check_move_test "white knight cannot make an illegal move from start" (4, 6)
      (Pieces.init Knight White (1, 7))
      init White false;
    check_move_test "right white knight cannot move to occupied from start"
      (2, 5)
      (Pieces.init Knight White (1, 7))
      init White false;
    check_move_test "left white knight cannot move to occupied from start" (2, 4)
      (Pieces.init Knight White (1, 2))
      init White false;
    (let board_w_knight_in_middle =
       update_board init (5, 4) (Pieces.init Knight White (1, 2))
     in
     check_move_test
       "left white knight in middle of board can move backwards in side L to \
        left"
       (4, 2)
       (Pieces.init Knight White (5, 4))
       board_w_knight_in_middle White true);
    (let board_w_knight_in_middle =
       update_board init (5, 4) (Pieces.init Knight White (1, 2))
     in
     check_move_test
       "left white knight in middle of board can move backwards in upright L \
        to left"
       (3, 3)
       (Pieces.init Knight White (5, 4))
       board_w_knight_in_middle White true);
    (let board_w_knight_in_middle =
       update_board init (5, 4) (Pieces.init Knight White (1, 2))
     in
     check_move_test
       "left white knight in middle of board can move backwards in side L to \
        right"
       (4, 6)
       (Pieces.init Knight White (5, 4))
       board_w_knight_in_middle White true);
    (let board_w_knight_in_middle =
       update_board init (5, 4) (Pieces.init Knight White (1, 2))
     in
     check_move_test
       "left white knight in middle of board can move backwards in upright L \
        to right"
       (3, 5)
       (Pieces.init Knight White (5, 4))
       board_w_knight_in_middle White true);
    check_move_test "knight cannot move straight" (3, 2)
      (Pieces.init Knight White (1, 2))
      init White false;
    (let board_w_knight_in_middle =
       update_board init (5, 4) (Pieces.init Knight White (1, 2))
     in
     check_move_test
       "left white knight in middle of board canot move backwards in side L to \
        left that is too large"
       (3, 2)
       (Pieces.init Knight White (5, 4))
       board_w_knight_in_middle White false);
    check_move_test "rook cannot hop over pieces" (5, 1)
      (Pieces.init Rook White (1, 1))
      init White false;
    (let free_rook = update_board init (2, 1) (Pieces.init Rook White (1, 1)) in
     check_move_test "rook can move straight forward as long as free" (5, 1)
       (Pieces.init Rook White (2, 1))
       free_rook White true);
    (let free_rook = update_board init (3, 1) (Pieces.init Rook White (1, 1)) in
     check_move_test "rook can move straight side as long as free" (3, 5)
       (Pieces.init Rook White (3, 1))
       free_rook White true);
    (let middle_rook =
       update_board init (4, 4) (Pieces.init Rook White (1, 1))
     in
     check_move_test "rook can move straight left as long as free" (4, 1)
       (Pieces.init Rook White (4, 4))
       middle_rook White true);
    (let middle_rook =
       update_board init (4, 4) (Pieces.init Rook White (1, 1))
     in
     check_move_test "rook can move straight down 1 as long as free" (3, 4)
       (Pieces.init Rook White (4, 4))
       middle_rook White true);
    (let obstr = update_board init (4, 4) (Pieces.init Rook White (1, 1)) in
     let middle_rook_obstructed =
       update_board obstr (4, 2) (Pieces.init Pawn White (2, 2))
     in
     check_move_test "rook can move straight left as long as free" (4, 1)
       (Pieces.init Rook White (4, 4))
       middle_rook_obstructed White false);
    (let obstr = update_board init (5, 4) (Pieces.init Rook White (1, 1)) in
     let middle_rook_obstructed =
       update_board obstr (4, 4) (Pieces.init Pawn White (2, 4))
     in
     check_move_test "rook can move straight left as long as free" (3, 4)
       (Pieces.init Rook White (5, 4))
       middle_rook_obstructed White false);
    (let free_down = update_board init (5, 4) (Pieces.init Rook White (1, 1)) in
     check_move_test "rook can move straight down many steps as long as free"
       (3, 4)
       (Pieces.init Rook White (5, 4))
       free_down White true);
    (let free_rook = update_board init (5, 1) (Pieces.init Rook White (1, 1)) in
     let obstr =
       update_board free_rook (5, 3) (Pieces.init Pawn White (2, 3))
     in
     check_move_test "rook can move straight right as long as free" (5, 5)
       (Pieces.init Rook White (5, 1))
       obstr White false);
    check_move_test "rook can only move straight" (2, 2)
      (Pieces.init Rook White (5, 1))
      init White false;
    check_move_test "bishop can only move diagonal" (1, 1)
      (Pieces.init Bishop White (1, 3))
      init White false;
    check_move_test "bishop cannot move NE if blocked" (3, 8)
      (Pieces.init Bishop White (1, 6))
      init White false;
    (let free_bishop =
       update_board init (3, 5) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop cannot move SE if blocked" (1, 3)
       (Pieces.init Bishop White (3, 5))
       free_bishop White false);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally NE" (6, 5)
       (Pieces.init Bishop White (5, 4))
       free_bishop White true);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally NE until blocked" (8, 6)
       (Pieces.init Bishop White (5, 4))
       free_bishop White false);
    check_move_test "bishop cannot move NE if blocked" (2, 5)
      (Pieces.init Bishop White (1, 6))
      init White false;
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally SE until blocked" (2, 1)
       (Pieces.init Bishop White (5, 4))
       free_bishop White false);
    (let free_bishop =
       update_board init (3, 5) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop cannot move diagonally SW if blocked" (3, 1)
       (Pieces.init Bishop White (3, 5))
       free_bishop White false);
    (let free_bishop =
       update_board init (3, 5) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop cannot move diagonally SE if blocked" (2, 6)
       (Pieces.init Bishop White (3, 5))
       free_bishop White false);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally SE " (3, 2)
       (Pieces.init Bishop White (5, 4))
       free_bishop White true);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally NW " (6, 3)
       (Pieces.init Bishop White (5, 4))
       free_bishop White true);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally NW " (7, 2)
       (Pieces.init Bishop White (5, 4))
       free_bishop White true);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally NW until it's blocked" (8, 1)
       (Pieces.init Bishop White (5, 4))
       free_bishop White false);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally SW " (3, 2)
       (Pieces.init Bishop White (5, 4))
       free_bishop White true);
    check_move_test "king cant move 2 forward" (3, 5)
      (Pieces.init King White (1, 5))
      init White false;
    check_move_test "king cant move 2 diagonal right" (3, 6)
      (Pieces.init King White (1, 5))
      init White false;
    check_move_test "king cant move 2 left" (1, 1)
      (Pieces.init King White (1, 5))
      init White false;
    (let board = init in
     print_board (to_lst board);
     check_move_test "king cant move 2 left" (1, 6)
       (Pieces.init King White (1, 5))
       board White false);
    check_move_test "king cant move 2 diagonal left" (3, 6)
      (Pieces.init King White (1, 5))
      init White false;
    check_move_test "king cant teleport to a random place" (5, 1)
      (Pieces.init King White (1, 4))
      init White false;
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 forward" (6, 4)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 backward" (4, 4)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 right" (5, 5)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 left" (5, 3)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 diagonal top right" (6, 5)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 diagonal top left" (6, 3)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 diagonal bottom left" (4, 3)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 diagonal bottom right" (4, 5)
       (Pieces.init King White (5, 4))
       free_king White true);
    check_move_test "queen moving unaturally" (3, 5)
      (Pieces.init Queen White (1, 4))
      init White false;
    (let free_queen =
       update_board init (4, 4) (Pieces.init Queen White (1, 4))
     in
     check_move_test "Queen moving diagonally bottom left" (3, 3)
       (Pieces.init Queen White (4, 4))
       free_queen White true);
    (let free_queen =
       update_board init (4, 4) (Pieces.init Queen White (1, 4))
     in
     check_move_test "Queen moving diagonally bottom right" (3, 5)
       (Pieces.init Queen White (4, 4))
       free_queen White true);
    (let free_queen =
       update_board init (4, 4) (Pieces.init Queen White (1, 4))
     in
     check_move_test "Queen moving forwards " (5, 4)
       (Pieces.init Queen White (4, 4))
       free_queen White true);
    (let free_queen =
       update_board init (4, 4) (Pieces.init Queen White (1, 4))
     in
     check_move_test "Queen moving backwards " (3, 4)
       (Pieces.init Queen White (4, 4))
       free_queen White true);
    (let free_queen =
       update_board init (4, 4) (Pieces.init Queen White (1, 4))
     in
     check_move_test "Queen moving right " (4, 5)
       (Pieces.init Queen White (4, 4))
       free_queen White true);
    check_mate_test "checkmate with two rooks" check_mate_two_rooks White true;
    check_mate_test "checkmate with queen and king" check_mate_queen_king Black
      true;
    check_mate_test "checkmate with rook and king" check_mate_rook_king Black
      true;
    check_mate_test "fool's checkmate. Black checkmates white" fools_checkmate
      White true;
    check_mate_test "smothered checkmate. White checkmates black"
      smothered_checkmate Black true;
    check_test "White knight checking black king variation 1" knight_check_1
      Black true;
    check_test "White knight checking black king variation 2" knight_check_2
      Black true;
    check_test "White knight checking black king variation 3" knight_check_3
      Black true;
    check_test "White knight checking black king variation 4" knight_check_4
      Black true;
    check_test "White knight checking black king variation 5" knight_check_5
      Black true;
    check_test "White knight checking black king variation 6" knight_check_6
      Black true;
    check_test "White knight checking black king variation 7" knight_check_7
      Black true;
    check_test "White knight checking black king variation 8" knight_check_8
      Black true;
    check_test "initial board shouldn't have check for Black" init Black false;
    check_test "initial board shouldn't have check for white" init White false;
    check_test "White Rook checking black king variation 1" rook_check_1 Black
      true;
    check_test "White Rook checking black king variation 2" rook_check_2 Black
      true;
    check_test "White Rook checking black king variation 3" rook_check_3 Black
      true;
    check_test "White Rook checking black king variation 4" rook_check_4 Black
      true;
    check_test "White Bishop checking black king variation 1" bishop_check_1
      Black true;
    check_test "White Bishop checking black king variation 2" bishop_check_2
      Black true;
    check_test "White Bishop checking black king variation 3" bishop_check_3
      Black true;
    check_test "White Bishop checking black king variation 4" bishop_check_4
      Black true;
    check_test "White Queen checking black king variation 1" queen_check_1 Black
      true;
    check_test "White Queen checking black king variation 2" queen_check_2 Black
      true;
    check_test "White Queen checking black king variation 3" queen_check_3 Black
      true;
    check_test "White Queen checking black king variation 4" queen_check_4 Black
      true;
    check_test "White Queen checking black king variation 5" queen_check_5 Black
      true;
    check_test "White Queen checking black king variation 6" queen_check_6 Black
      true;
    check_test "White Queen checking black king variation 7" queen_check_7 Black
      true;
    check_test "White Queen checking black king variation 8" queen_check_8 Black
      true;
    check_test "White pawn checking black king variation 1" pawn_check_1 Black
      true;
    check_test "White pawn checking black king variation 2" pawn_check_2 Black
      true;
    check_test "White pawn cannot check king if it is in front" pawn_check_3
      Black false;
    check_test "White King checking Black king variation 1" king_check_1 Black
      true;
    check_test "White King checking Black king variation 2" king_check_2 Black
      true;
    check_test "White King checking Black king variation 3" king_check_3 Black
      true;
    check_test "White King checking Black king variation 4" king_check_4 Black
      true;
    check_test "White King checking Black king variation 5" king_check_5 Black
      true;
    check_test "White King checking Black king variation 6" king_check_6 Black
      true;
    check_test "White King checking Black king variation 7" king_check_7 Black
      true;
    check_test "White King checking Black king variation 8" king_check_8 Black
      true;
    invalid_move_into_valid_move;
  ]

let suite =
  "test suite for Chess Game"
  >::: List.flatten [ board_tests; command_tests; logic_tests ]

let _ = run_test_tt_main suite