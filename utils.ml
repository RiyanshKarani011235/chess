open Abstract_syntax

let abs = fun (num : int) : int -> 
  if num < 0 then (-num)
  else num

(* checks if a move is along a file - vertical *)
let is_move_along_file = fun (src : square) (dst : square) : bool -> 
  let (src_file, src_rank) = src in
  let (dst_file, dst_rank) = dst in
  src_file == dst_file && src_rank != dst_rank

(* checks if a move is along a rank - horizontal *)
let is_move_along_rank = fun (src : square) (dst : square) : bool -> 
  let (src_file, src_rank) = src in
  let (dst_file, dst_rank) = dst in
  src_file != dst_file && src_rank == dst_rank

(* checks if a move is along a diagonal *)
let is_move_along_diagonal = fun (src : square) (dst : square) : bool -> 
  let (src_file, src_rank) = src in
  let (dst_file, dst_rank) = dst in
  (* num increase / decrease in file should be equal to the num 
  increase/decrease in the rank *)
  abs (src_file - dst_file) == abs (src_rank - dst_rank)

(* checks if the given move is what the piece can make *)
(* this method does not take the board configuration in consideration *)
(* assumes that the square is a valid square *)
let is_valid_piece_move = fun (src : square) (dst : square) (p : piece) : bool ->
  let (src_file, src_rank) = src in
  let (dst_file, dst_rank) = dst in
  match p with
    | King ->
      (* single step along file *)
      if is_move_along_file src dst && abs (src_rank - dst_rank) == 1 then true
      (* single step along rank *)
      else if is_move_along_rank src dst && abs (src_file - dst_file) == 1 then true
      (* invalid move *)
      else false
    | Queen -> is_move_along_file src dst || is_move_along_rank src dst || is_move_along_diagonal src dst
    | Rook -> is_move_along_file src dst || is_move_along_rank src dst
    | Bishop -> is_move_along_diagonal src dst
    | Knight -> 
      (* file change by 2 and rank change by 1 *)
      if abs (src_file - dst_file) == 2 && abs (src_rank - dst_rank) == 1 then true
      (* file change by 1 and rank change by 2 *)
      else if abs (src_file - dst_file) == 1 && abs (src_rank - dst_rank) == 2 then true
      (* invalid move *)
      else false
    | Pawn -> 
      (* should always move along a rank *)
      if dst_rank - src_rank == 1 then
        (* non-capture move *)
        if is_move_along_file src dst then true
        (* capture move *)
        else if is_move_along_diagonal src dst && abs (src_file - dst_file) == 1 then true
        else false
      else false

(* get next square from src in the direction from src to dst*)
(* horizontally, vertically or diagonally *)
let get_next_square = fun (src : square) (dst : square) : square -> 
  let (src_file, src_rank) = src in
  let (dst_file, dst_rank) = dst in

  if src_rank == dst_rank && src_file != dst_file then
    (* horizontal *)
    if src_file < dst_file then
      (* right *)
      (src_file + 1, src_rank)
    else 
      (* left *)
      (src_file - 1, src_rank)
  else if src_rank != dst_rank && src_file == dst_file then
    (* vertical *)
    if src_rank < dst_rank then
      (* up *)
      (src_file, src_rank + 1)
    else 
      (* down *)
      (src_file, src_rank - 1)
  else if is_move_along_diagonal src dst then
    (* diagonal *)
    if src_file < dst_file && src_rank < dst_rank then
      (* right & up *)
      (src_file + 1, src_rank + 1)
    else if src_file < dst_file && src_rank > dst_rank then
      (* right & down *)
      (src_file + 1, src_rank - 1)
    else if src_file > dst_file && src_rank < dst_rank then
      (* left & up *)
      (src_file - 1, src_rank + 1)
    else
      (* left & down *)
      (src_file - 1, src_rank - 1)
  else
    (* invalid *)
    raise Exceptions.RunTimeException
