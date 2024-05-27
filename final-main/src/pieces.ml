type name =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type zcolor =
  | Black
  | White

type t = {
  name : name;
  loc : int * int;
  color : zcolor;
  moved : bool;
}

exception Wrong

let init name color loc : t = { name; loc; color; moved = false }

let update_location (t : t) (new_loc : int * int) =
  { t with loc = new_loc; moved = true }

let location t = t.loc
let name t = t.name

let to_string t =
  match name t with
  | Pawn -> "pawn"
  | Knight -> "knight"
  | Bishop -> "bishop"
  | Rook -> "rook"
  | Queen -> "queen"
  | King -> "king"

let from_string str =
  match str with
  | "pawn" -> Pawn
  | "knight" -> Knight
  | "bishop" -> Bishop
  | "rook" -> Rook
  | "queen" -> Queen
  | "king" -> King
  | _ -> raise Wrong

let color_to_string color =
  match color with
  | Black -> "Black"
  | White -> "White"

let moved t = t.moved
let color t = t.color
