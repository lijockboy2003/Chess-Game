(** Checks that moves are valid and evaluates the status of the game.contents

    This module processes whether or not moves are valid based on the type of
    the piece. It processes normal moves and castles. It also checks if any of
    the players are in check, checkmate, or if the game is at a stalemate. *)

type direction =
  | Left
  | Right

(** [direction] is the direction in which the king is castling*)

exception Castle of direction * Pieces.t * Pieces.t
(** Raised to indicate that a king is castling*)

val check_move : int * int -> Pieces.t -> Board.board -> Pieces.zcolor -> bool
(** [check_move move info board color] is if a [color] piece with information
    [info] can legally move to [move] on the given [board], disregarding
    potential for checks, checkmates, and stalemates. The only exception to this
    final point is if castling causes the king to be in check at any point.*)

val check : Board.board -> Pieces.zcolor -> bool
(** [check board color] returns whether or not the [color] player is in check. *)

val check_mate : Board.board -> Pieces.zcolor -> bool
(** [check_mate board color] returns whether or not the [color] player is in
    checkmate. *)

val stalemate : Board.board -> bool
(** [stalemate board] is if the [board] represents a game that only has two
    kings on the board, which would result in an automatic stalemate.*)
