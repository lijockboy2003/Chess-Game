(** Parses the command given by the user into values that can be processed. *)

exception Invalid
(** Raised to signal that the given input doesn't follow the specified
    formatting.*)

type object_phrase = {
  piece : Pieces.t;
  move : int * int;
}
(** [object_phrase] is the type of the output, which contains the piece being
    moved and where it's being moved. *)

val parse : string -> Board.board -> object_phrase
(** [parse str board] converts the numbers in [str] to an object_phrase. If
    [str] is not in the correct format, an exception Invalid is raised.*)

val move : object_phrase -> int * int
(** [move command] is the move stored inside [command].*)

val piece : object_phrase -> Pieces.t
(** [piece command] is the piece stored inside [command].*)
