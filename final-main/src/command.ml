open Board
open Pieces

type object_phrase = {
  piece : t;
  move : int * int;
}

exception Invalid

let parse str board =
  let x = String.split_on_char ' ' str in
  let lst = List.filter (fun y -> y <> "") x in
  match lst with
  | [] -> raise Invalid
  | [ u1; u2; u3; u4 ] ->
      let piece = find_piece (int_of_string u1, int_of_string u2) board in
      { piece; move = (int_of_string u3, int_of_string u4) }
  | _ -> raise Invalid

let move command = command.move
let piece command = command.piece