(** Representation of the board

    This module represents the chess board, specifically the status of each tile
    which is either empty or full. If the tile is full, the piece located at the
    specific tile is stored*)

type tile =
  | Empty of int * int
  | Full of int * int * Pieces.t

(** Represents the type of a singular tile of the board, which is either empty
    or a tuple of two integers *)

exception MissingPiece
(** Raised to signal that a tile does not have a piece on it*)

type board = tile list
(** Represents the type of the board, which is represented a list of tiles *)

val empty_board : tile list ref -> unit
(** [empty_board lst] intializes an empty board without any pieces*)

val init : board
(** [init] creates a starting board*)

val valid_move : board -> int * int -> Pieces.zcolor -> bool
(** [valid_move board move color] determines if a space is occupied or not based
    off of the given [board] and the inputed [move]*)

val update_board : board -> int * int -> Pieces.t -> board
(** [update_board board move piece] updates the board given an old board, move,
    and a piece *)

val find_piece : int * int -> board -> Pieces.t
(** [find_piece coord board] finds the piece on the [board] given the [coord]
    and particular [board]*)

val to_string : board -> string
(** [to_string board] converts [board] to a string for the command line*)

val to_lst : board -> string list list
(** [to_lst board] converts [board] into a list of string lists which is printed
    to the terminal command interface*)

val is_empty : board -> int * int -> bool
(** [is_empty board coord] states whether or not [coord] on [board] is empty*)

val isolate_black : board -> board
(** [isolate_black brd] converts [brd] to a board with only the black pieces*)

val isolate_white : tile list -> board
(** [isolate_white brd] converts [brd] to a board with only the white pieces*)