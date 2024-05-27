open Pieces
open Board

type direction =
  | Left
  | Right

exception Castle of direction * Pieces.t * Pieces.t

let pawn_reachable pos tile =
  let loc = location tile in
  let color = color tile in
  if color = White then
    match (pos, loc) with
    | (u1, u2), (t1, t2) ->
        if u2 = t2 then false
        else if (u2 = t2 + 1 || u2 = t2 - 1) && u1 = t1 + 1 then true
        else false
  else
    match (pos, loc) with
    | (u1, u2), (t1, t2) ->
        if u2 = t2 then
          if u1 = t1 - 1 then true else if u1 = t1 - 2 then true else false
        else if (u2 = t2 + 1 || u2 = t2 - 1) && u1 = t1 - 1 then true
        else false

let knight_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) ->
      if u2 = t2 + 1 || u2 = t2 - 1 then
        if u1 = t1 + 2 || u1 = t1 - 2 then true else false
      else if u2 = t2 + 2 || u2 = t2 - 2 then
        if u1 = t1 + 1 || u1 = t1 - 1 then true else false
      else false

let rook_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) -> if u2 = t2 || u1 = t1 then true else false

let bishop_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) ->
      if Int.abs (t2 - u2) = Int.abs (t1 - u1) || t2 - u2 = u1 - t1 then true
      else false

let queen_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) ->
      if Int.abs (t2 - u2) = Int.abs (t1 - u1) || u1 = t1 || u2 = t2 then true
      else false

let king_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) ->
      if u1 = t1 then if u2 = t2 + 1 || u2 = t2 - 1 then true else false
      else if u2 = t2 then if u1 = t1 + 1 || u1 = t1 - 1 then true else false
      else if u2 = t2 + 1 || u2 = t2 - 1 then
        if u1 = t1 + 1 || u1 = t1 - 1 then true else false
      else false

let find_king c ti =
  match ti with
  | Full (_, _, z) -> (
      match name z with
      | King -> if color z = c then true else false
      | _ -> false)
  | _ -> false

let check_check pos tile =
  match tile with
  | Full (_, _, piece) -> (
      match name piece with
      | Pawn -> pawn_reachable pos piece
      | Knight -> knight_reachable pos piece
      | Rook -> rook_reachable pos piece
      | Bishop -> bishop_reachable pos piece
      | Queen -> queen_reachable pos piece
      | King -> king_reachable pos piece)
  | _ -> raise Not_found

(*returns true iff the king is in check*)
let rec check_helper (board : board) pos : bool =
  match board with
  | [] -> false
  | h :: t -> check_check pos h || check_helper t pos

let check (board : tile list) color =
  let king = List.find (find_king color) board in
  match king with
  | Full (_, _, k) ->
      let king_position = location k in
      if color = Black then check_helper (isolate_white board) king_position
      else check_helper (isolate_black board) king_position
  | _ -> raise Not_found

(*returns true iff the king can move*)
let rec check_king_moves move_lst info board color =
  match move_lst with
  | [] -> false
  | h :: t ->
      let can_move =
        if
          check_move h info board color
          && not (check (update_board board h info) color)
        then true
        else false
      in
      can_move || check_king_moves t info board color

and check_mate board color =
  let king = List.find (find_king color) board in
  let king_info =
    match king with
    | Full (_, _, info) -> info
    | Empty (_, _) -> raise Not_found
  in
  let king_postion = location king_info in
  let possible_king_position_total =
    match king_postion with
    | x, y ->
        [
          (x + 1, y);
          (x, y + 1);
          (x + 1, y + 1);
          (x - 1, y);
          (x, y - 1);
          (x - 1, y - 1);
          (x + 1, y - 1);
          (x - 1, y + 1);
        ]
  in
  let possible_king_position =
    List.filter
      (fun (x1, y2) ->
        if x1 > 8 || x1 < 1 then false
        else if y2 > 8 || y2 < 1 then false
        else true)
      possible_king_position_total
  in
  (not (check_king_moves possible_king_position king_info board color))
  && check board color

(*Psuedocode: store the position of the king in a variable named king_position.
  Then, using king_position as a parameter call*)

and pawn_move (move : int * int) info board color =
  let loc = location info in
  if color = White then
    match (move, loc) with
    | (u1, u2), (t1, t2) ->
        if u2 = t2 then
          if u1 = t1 + 1 then is_empty board move
          else if u1 = t1 + 2 then is_empty board move && not (moved info)
          else false
        else if (u2 = t2 + 1 || u2 = t2 - 1) && u1 = t1 + 1 then
          valid_move board move color && not (is_empty board move)
        else false
  else
    match (move, loc) with
    | (u1, u2), (t1, t2) ->
        if u2 = t2 then
          if u1 = t1 - 1 then is_empty board move
          else if u1 = t1 - 2 then is_empty board move && not (moved info)
          else false
        else if (u2 = t2 + 1 || u2 = t2 - 1) && u1 = t1 - 1 then
          valid_move board move color && not (is_empty board move)
        else false

and knight_move move info board color =
  let loc = location info in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if u2 = t2 + 1 || u2 = t2 - 1 then
        if u1 = t1 + 2 || u1 = t1 - 2 then valid_move board move color
        else false
      else if u2 = t2 + 2 || u2 = t2 - 2 then
        if u1 = t1 + 1 || u1 = t1 - 1 then valid_move board move color
        else false
      else false

and rook_move move info board color =
  let loc = location info in
  let output = ref (valid_move board move color) in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if u2 = t2 then
        if u1 < t1 then
          for x = u1 + 1 to t1 - 1 do
            if !output && is_empty board (x, u2) then output := true
            else output := false
          done
        else
          for x = t1 + 1 to u1 - 1 do
            if !output && is_empty board (x, u2) then output := true
            else output := false
          done
      else if u1 = t1 then
        if u2 < t2 then
          for x = u2 + 1 to t2 - 1 do
            if !output && is_empty board (u1, x) then output := true
            else output := false
          done
        else
          for x = t2 + 1 to u2 - 1 do
            if !output && is_empty board (u1, x) then output := true
            else output := false
          done
      else output := false;
      !output

and bishop_move move info board color =
  let loc = location info in
  let output = ref (valid_move board move color) in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      let x1 = ref u1 in
      let x2 = ref t1 in
      (*positive slope diagonal*)
      if t2 - u2 = t1 - u1 then
        if t2 > u2 then (
          x1 := !x1 + 1;
          for x = u2 + 1 to t2 - 1 do
            if not (is_empty board (!x1, x)) then output := false;
            x1 := !x1 + 1
          done)
        else (
          x2 := !x2 + 1;
          for x = t2 + 1 to u2 - 1 do
            if not (is_empty board (!x2, x)) then output := false;
            x2 := !x2 + 1
          done)
      else if t2 - u2 = u1 - t1 then
        if t2 > u2 then (
          x1 := !x1 - 1;
          for x = u2 + 1 to t2 - 1 do
            if not (is_empty board (!x1, x)) then output := false;
            x1 := !x1 - 1
          done)
        else (
          x2 := !x2 - 1;
          for x = t2 + 1 to u2 - 1 do
            if not (is_empty board (!x2, x)) then output := false;
            x2 := !x2 - 1
          done)
      else output := false;
      !output

and queen_move move info board color =
  let loc = location info in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if t2 - u2 = t1 - u1 || t2 - u2 = u1 - t1 then
        valid_move board move color && bishop_move move info board color
      else if u1 = t1 || u2 = t2 then
        valid_move board move color && rook_move move info board color
      else false

and check_castle info board color dir =
  try
    let t1, t2 = location info in
    if dir = Right then
      let rook = find_piece (t1, t2 + 3) board in
      if
        (not (moved info))
        && is_empty board (t1, t2 + 1)
        && is_empty board (t1, t2 + 2)
        && (not (check (update_board board (t1, t2 + 1) info) color))
        && (not (check (update_board board (t1, t2 + 2) info) color))
        && name rook = Rook
        && not (moved rook)
      then raise (Castle (Right, info, rook))
      else false
    else
      let rook = find_piece (t1, t2 - 4) board in
      if
        (not (moved info))
        && is_empty board (t1, t2 - 1)
        && is_empty board (t1, t2 - 2)
        && (not (check (update_board board (t1, t2 - 1) info) color))
        && (not (check (update_board board (t1, t2 - 2) info) color))
        && name rook = Rook
        && not (moved rook)
      then raise (Castle (Left, info, rook))
      else false
  with MissingPiece -> false

and king_move move info board color =
  let loc = location info in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if u1 = t1 then
        if u2 = t2 + 1 || u2 = t2 - 1 then valid_move board move color
        else if t2 = u2 - 2 then check_castle info board color Right
        else if t2 = u2 + 2 then check_castle info board color Left
        else false
      else if u2 = t2 then
        if u1 = t1 + 1 || u1 = t1 - 1 then valid_move board move color
        else false
      else if u2 = t2 + 1 || u2 = t2 - 1 then
        if u1 = t1 + 1 || u1 = t1 - 1 then valid_move board move color
        else false
      else false

and check_move move info board color =
  if move = location info then false
  else
    match name info with
    | Pawn -> pawn_move move info board color
    | Knight -> knight_move move info board color
    | Rook -> rook_move move info board color
    | Bishop -> bishop_move move info board color
    | Queen -> queen_move move info board color
    | King -> king_move move info board color

let stalemate board =
  let pieces = ref 0 in
  for x = 1 to 8 do
    for y = 1 to 8 do
      try if name (find_piece (x, y) board) <> King then pieces := 1
      with MissingPiece -> pieces := !pieces
    done
  done;
  if !pieces = 0 then true else false