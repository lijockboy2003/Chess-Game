(** Representation of pieces

    This module represents the pieces that appear on a chess board, specifically
    its color, piece type, current position on the board, and whether or not it
    has been moved yet.*)

exception Wrong
(** Raised when a string can't correspond to a name*)

type name =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

(** Represents the type of pieces that exist on a chess board.*)

type zcolor =
  | Black
  | White

(** Represents the possible colors a piece can have on a chess board.*)

type t
(** The abstract type that represents a piece. A piece should have a name,
    value, current location, color, and whether it has moved or not *)

val init : name -> zcolor -> int * int -> t
(** [init name color loc] initializes a piece given its [name], [color], and
    [loc]. It is assumed that the piece has not been moved.*)

val update_location : t -> int * int -> t
(** [update_location t new_loc] updates the location of [t] after making a move
    to [new_loc].*)

val location : t -> int * int
(** [location t] returns the current location of [t].*)

val name : t -> name
(** [name t] returns the name of [t]. *)

val to_string : t -> string
(** [to_string t] converts [t] to its string format. *)

val from_string : string -> name
(**[from_string str] convers [str] to its name format. *)

val moved : t -> bool
(** [moved t] is whether or not [t] has been previously moved yet. *)

val color : t -> zcolor
(** [color t] is the color of [t].*)

val color_to_string : zcolor -> string
(** [color_to_string] returns the color of [t] in string formatting.*)
