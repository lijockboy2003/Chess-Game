open Game
open Board
open Command
open Logic
open Pieces

(** The type representing the possible game outcomes.*)
type outcome =
  | Black
  | White
  | Draw

(** The type representing the game's statistics. It includes the name of hte
    players, their scores, and the color they're playing in the current game.*)

type name = {
  p1 : string;
  p2 : string;
  p1score : int;
  p2score : int;
  p1color : Pieces.zcolor;
  p2color : Pieces.zcolor;
}

exception GameOver of outcome * name
(**Raised when the game ends*)

(**[print_helper v] is a helper function for [print_board].*)
let rec print_helper = function
  | [] -> print_string ""
  | u1 :: u2 ->
      print_string (u1 ^ " ");
      print_helper u2

(**[print_board i] is a helper function for [print].*)
let rec print_board i = function
  | [] ->
      print_endline "";
      print_endline ("\027[34m  1 2 3 4 5 6 7 8" ^ "\027[0m");
      print_endline ""
  | u1 :: u2 ->
      print_endline "";
      print_string ("\027[34m" ^ string_of_int i ^ " " ^ "\027[0m");
      print_helper u1;
      print_board (i - 1) u2

(**[print board] prints [board].*)
let print board = print_board 8 (to_lst board)

(**[game_calc outcome name] calculates the new score based on the [outcome].*)
let rec game_calc outcome name =
  match outcome with
  | Black ->
      print_endline "Good game!";
      if name.p1color = Black then { name with p1score = name.p1score + 1 }
      else { name with p2score = name.p2score + 1 }
  | White ->
      print_endline "Good game!";
      if name.p1color = White then { name with p1score = name.p1score + 1 }
      else { name with p2score = name.p2score + 1 }
  | Draw ->
      print_endline "Good game!";
      name

(**[game_over_help name] processes the new command and acts accordingly. It only
   accepts two commands. If the user has typed in the name of the player, this
   triggers a new game with the name as white. If the player types quit, the
   game exits.*)
and game_over_help name =
  try
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "quit" ->
        print_endline "Goodbye! Thanks for playing!";
        exit 0
    | "demo menu" -> demo name
    | white ->
        if
          (white = name.p1 && name.p1color = White)
          || (white = name.p2 && name.p2color = White)
        then multiplayer Board.init name
        else if white = name.p1 && name.p1color = Black then
          multiplayer Board.init { name with p1color = White; p2color = Black }
        else if white = name.p2 && name.p2color = Black then
          multiplayer Board.init { name with p2color = White; p1color = Black }
        else raise Invalid
  with Invalid ->
    print_endline "Please enter a valid name.";
    game_over_help name

(**[game_over outcome name] states the new score and asks the user to play again
   or quit.*)
and game_over outcome name =
  let name = game_calc outcome name in
  Printf.printf "The new score is %s: %i pts; %s: %i pts." name.p1 name.p1score
    name.p2 name.p2score;
  print_endline "";
  print_endline
    "To play again, type the name of player who wishes to play as white. If \
     you want to quit, you can just type quit";
  game_over_help name

(**[check_msg names color] is a special print method that warns player of
   [color] that they are in check.*)
and check_msg names (color : Pieces.zcolor) =
  if color = Black then
    match names.p1color with
    | White -> Printf.printf "%s, you are in check" names.p2
    | Black -> Printf.printf "%s, you are in check" names.p1
  else
    match names.p1color with
    | White -> Printf.printf "%s, you are in check" names.p1
    | Black -> Printf.printf "%s, you are in check" names.p2

(**[process_helper board names color] checks for special commands, such as
   "quit", "forfeit", and "stalemate". If the command is not listed, then it
   passes the command onto [process].*)
and process_helper board names (color : Pieces.zcolor) =
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" ->
      print_endline "Goodbye! Thanks for playing!";
      exit 0
  | "forfeit" ->
      if color = White then game_over Black names else game_over White names
  | "stalemate" -> game_over Draw names
  | "board" ->
      print board;
      process_helper board names color
  | "score" ->
      Printf.printf "The score is %s: %i pts; %s: %i pts." names.p1
        names.p1score names.p2 names.p2score;
      print_endline "";
      process_helper board names color
  | "demo menu" -> demo names
  | move -> process move board names color

(**[castle_helper d king rook move board names] processes castles based on its
   direction [d].*)
and castle_helper d king rook move board names =
  let king_board = update_board board move king in
  match (d, move) with
  | Left, (t1, t2) ->
      let new_board = update_board king_board (t1, t2 + 1) rook in
      print new_board;
      if color king = White then
        proc_move new_board names (Black : Pieces.zcolor)
      else proc_move new_board names White
  | Right, (t1, t2) ->
      let new_board = update_board king_board (t1, t2 - 1) rook in
      print new_board;
      if color king = White then
        proc_move new_board names (Black : Pieces.zcolor)
      else proc_move new_board names White

and promotion move board color p : board =
  print_string "> ";
  match read_line () with
  | exception End_of_file -> promotion move board color p
  | "king" ->
      print_endline "Sorry, you can't promote to a king";
      print_endline "Please enter another piece.";
      promotion move board color p
  | "pawn" ->
      print_endline "Sorry, you can't promote to a pawn";
      print_endline "Please enter another piece.";
      promotion move board color p
  | piece -> (
      try
        update_board board move
          (Pieces.init (from_string piece) color (location p))
      with Wrong ->
        print_endline "Please enter a valid piece.";
        promotion move board color p)

(**[process move board names color] parses [move] and checks if it is a valid
   command. If not, it prompts the user for new input.*)
and process move board names color : unit =
  try
    let new_move = parse move board in
    let new_board =
      update_board board (Command.move new_move) (piece new_move)
    in
    if check new_board color then raise Invalid
    else if check_move new_move.move new_move.piece board color then (
      if name (piece new_move) = Pawn then
        match new_move.move with
        | u1, _ ->
            if u1 = 8 || u1 = 1 then (
              print_endline
                "Your pawn is being promoted! What do you want your new piece \
                 to be?";
              let new_board =
                promotion new_move.move board color new_move.piece
              in
              print new_board)
            else print new_board
      else print new_board;
      if color = White then proc_move new_board names (Black : Pieces.zcolor)
      else proc_move new_board names White)
    else raise Invalid
  with
  | Invalid ->
      print_endline "Please enter a valid command.";
      process_helper board names color
  | MissingPiece ->
      print_endline "There is no piece in the first set of coordinates";
      print_endline
        "Double check your coordinates; remember to start with row first then \
         column";
      process_helper board names color
  | Castle (d, king, rook) ->
      castle_helper d king rook (Command.move (parse move board)) board names

(**[proc_move board names] checks if the [board] has any game-ending conditions,
   such as draws or checkmates. If there is a game ending condition, it throws
   exception GameOver. It also checks for checks to output a special message.
   After this, it prompts the player for a new input.*)
and proc_move board names (color : Pieces.zcolor) : unit =
  try
    if check_mate board color then
      if color = White then raise (GameOver (Black, names))
      else raise (GameOver (White, names));
    if stalemate board then raise (GameOver (Draw, names));
    if check board color then check_msg names color;
    print_endline " ";
    if names.p1color = color then Printf.printf "It's %s's turn." names.p1
    else Printf.printf "It's %s's turn." names.p2;
    print_endline "";
    process_helper board names color
  with GameOver (o, n) -> game_over o n

(** [multiplayer board names] starts the game on a given [board] and given
    [names]*)
and multiplayer board names =
  let n = if names.p1color = White then names.p1 else names.p2 in
  Printf.printf "Here is your current board. It is %s's move." n;
  print_endline "";
  print board;
  print_endline
    {|To make a move, type the location of the piece and where you want to move it, with row first then column, such as "2 2 3 2"|};
  print_endline {|To quit during any part of the game, type "quit".|};
  print_endline
    {|The game will automatically assume stalemate when there are only two kings left, but if at any point before that there is a draw please type "stalemate" to end the game.|};
  print_endline
    {|If you would like to resign, type "forfeit" during your turn.|};
  print_endline {|To see the board again, type "board".|};
  print_endline {|To see the score, type "score".|};
  process_helper board names White

(** [single_player board] is meant to show how the pieces move*)
and single_player board names =
  print board;
  print_endline " ";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "reset" -> solo_moves names
  | "demo menu" -> demo names
  | "quit" -> exit 0
  | move -> (
      try
        let new_move = parse move board in
        let new_board =
          update_board board (Command.move new_move) (piece new_move)
        in
        if check_move new_move.move new_move.piece board White then
          single_player new_board names
        else raise Invalid
      with _ -> single_player board names)

(** [solo_moves names] is a singleplayer freeplay version meant for the demo.*)
and solo_moves names =
  print_endline "Type in the name of the piece you would like to demo.";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "demo menu" -> demo names
  | "quit" -> exit 0
  | name -> (
      try
        single_player
          (update_board Board.init (4, 4)
             (Pieces.init (Pieces.from_string name) White (3, 3)))
          names
      with Wrong -> solo_moves names)

(**[tp_piece board move piece] moves a [piece] on the [board] to [move].*)
and tp_piece board (r, c) piece : board =
  let removed =
    List.filter
      (fun a ->
        match a with
        | Full (r', c', _) | Empty (r', c') ->
            if r = r' && c = c' then false else true)
      board
  in
  Full (r, c, piece) :: removed

(**[castle_moves names] is a multiplayer player freeplay version demonstrating
   how castling works.*)
and castle_moves names =
  let new_board = ref [] in
  empty_board new_board;
  let king_board = tp_piece !new_board (1, 5) (Pieces.init King White (1, 5)) in
  let rook_board = tp_piece king_board (1, 8) (Pieces.init Rook White (1, 8)) in
  let extra_piece =
    tp_piece rook_board (2, 1) (Pieces.init Pawn White (2, 1))
  in
  let black_rook =
    tp_piece extra_piece (8, 6) (Pieces.init Rook Black (8, 6))
  in
  let black_king = tp_piece black_rook (8, 1) (Pieces.init King Black (8, 1)) in
  print black_king;
  process_helper black_king names White

and promotion_demo names =
  let new_board = ref [] in
  empty_board new_board;
  let king_board = tp_piece !new_board (1, 5) (Pieces.init King White (1, 5)) in
  let promoting_pawn =
    tp_piece king_board (7, 1) (Pieces.init Pawn White (7, 1))
  in
  let black_king =
    tp_piece promoting_pawn (8, 5) (Pieces.init King Black (8, 5))
  in
  print black_king;
  process_helper black_king names White

(** [checkmate_rook names] creates the scenario where checkmate is about to
    happen for white via two rooks.*)
and checkmate_rook names =
  let res = ref [] in
  empty_board res;
  (* White king at D1*)
  let k1 = tp_piece !res (1, 4) (Pieces.init King White (1, 4)) in
  (* Black rook at A2*)
  let r1 = tp_piece k1 (2, 1) (Pieces.init Rook Black (2, 1)) in
  (* Black King at F5 *)
  let k2 = tp_piece r1 (5, 6) (Pieces.init King Black (5, 6)) in
  (* 2nd Black Rook rook at A8 *)
  let r2 = tp_piece k2 (8, 1) (Pieces.init Rook Black (8, 1)) in
  print r2;
  process_helper r2 names Black

(** [checkmate_queen names] creates the scenario where checkmate is about to
    happen for white via one queen.*)
and checkmate_queen names =
  let res = ref [] in
  empty_board res;
  (*Black King at G8*)
  let bk = tp_piece !res (8, 7) (Pieces.init King Black (8, 7)) in
  (*White King at G6 *)
  let wk = tp_piece bk (6, 7) (Pieces.init King White (6, 7)) in
  (*White queen at E1*)
  let wq = tp_piece wk (1, 5) (Pieces.init Queen White (1, 5)) in
  print wq;
  process_helper wq names White

(** [checkmate_rook names] creates the scenario where Black is currently in
    check.*)
and black_check names =
  let res = ref [] in
  empty_board res;
  let k1 = tp_piece !res (8, 4) (Pieces.init King Black (8, 4)) in
  let kn = tp_piece k1 (6, 3) (Pieces.init Knight White (6, 3)) in
  let p1 = tp_piece kn (7, 1) (Pieces.init Pawn Black (7, 1)) in
  let board = tp_piece p1 (1, 4) (Pieces.init King White (1, 4)) in
  print board;
  process_helper board names Black

(** [stalemate_demo names] creates the scenario where stalemate is about to
    happen.*)
and stalemate_demo names =
  let res = ref [] in
  empty_board res;
  let k1 = tp_piece !res (8, 4) (Pieces.init King Black (8, 4)) in
  let p1 = tp_piece k1 (7, 4) (Pieces.init Knight White (7, 4)) in
  let board = tp_piece p1 (1, 4) (Pieces.init King White (1, 4)) in
  print board;
  process_helper board names Black

(**[status_checking names] is a multiplayer freeplay version demonstrating
   various check / checkmate / stalemate scenarios.*)
and status_checking names =
  print_endline "Pick a scenario you want to demo";
  print_endline "1. Checkmate with two Rooks";
  print_endline "2. Checkmate with a Queen and a King";
  print_endline "3. Black is in check";
  print_endline "4. Stalemate";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "1" -> checkmate_rook names
  | "2" -> checkmate_queen names
  | "3" -> black_check names
  | "4" -> stalemate_demo names
  | _ -> status_checking names

(** [demo names] starts the demo version of the game*)
and demo names =
  print_endline "Please enter the number you want to demo";
  print_endline "1. Solo moves";
  print_endline "2. Castling";
  print_endline "3. Promotion";
  print_endline "4. Status checking";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "1" -> solo_moves names
  | "2" -> castle_moves names
  | "3" -> promotion_demo names
  | "4" -> status_checking names
  | _ -> demo names

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to our chess command line interface!\n\
    \ This is a multiplayer game.";
  print_endline " ";
  print_endline " ";
  print_endline "Please enter player one's name (they will start as white).";
  print_endline {|Alternatively, you can enter "demo" to enter the demo mode.|};
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "demo" ->
      print_endline
        "While this feature was created for the demo, you may use it to have \
         custom scenarios to play through.";
      print_endline " ";
      demo
        {
          p1 = "Owen";
          p2 = "Mo";
          p1score = 0;
          p2score = 0;
          p1color = White;
          p2color = Black;
        }
  | name -> (
      let names =
        {
          p1 = name;
          p2 = "No name";
          p1score = 0;
          p2score = 0;
          p1color = White;
          p2color = Black;
        }
      in
      print_endline "Please enter player two's name (they will start as black).";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | name -> multiplayer Board.init { names with p2 = name })

(* Execute the game engine. *)
let () = main ()
