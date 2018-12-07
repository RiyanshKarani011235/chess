open Abstract_syntax
open Utils

(* finds first piece between two squares that are aligned either *)
(* horizontally, vertically or diagonally *)
let rec find_first_piece_between = fun (src : square) (dst : square) (b : Board.board) -> 
  let (src_file, src_rank) = src in
  let (dst_file, dst_rank) = dst in
  
  (* if there is a piece at the current position, then return it *)
  (* else, search at the next square between the src and the dst *)
  let p = Hashtbl.find_opt b.square_piece_map src in
  match p with
    | None -> 
      (* no piece found at the current position *)
      (* if src is dst *)
      if src_file == dst_file then None

      (* if src is not dst *)
      else find_first_piece_between (get_next_square src dst) dst b
    | cp -> 
      (* piece found at the current position, return it *)
      cp

(* TODO: implement *)
let is_square_in_check = fun (s : square) (piece_color : color) (b : Board.board) : bool -> false

let is_valid_game_move = fun (src : square) (dst : square) (cp : colored_piece) (b : Board.board) : bool ->
  let (c, p) = cp in
  let (src_file, src_rank) = src in
  let (dst_file, dst_rank) = dst in
  let dst_cp = Hashtbl.find_opt b.square_piece_map dst in

  (* check if a move is actually being made *)
  not ((src_file == dst_file) && (src_rank == dst_rank))

  &&

  (* check if not valid piece move *)
  is_valid_piece_move src dst p 

  (* TODO: *)
  (* check if moving a piece leads to a check *)

  &&

  (* check if capturing a piece of the same color *)
  match dst_cp with
    (* not capturing a piece *)
    | None -> true
    (* capturing a piece *)
    | Some dst_cp_ -> let (dst_c, dst_p) =  dst_cp_ in
      dst_c != c

  &&

  (* piece specific checks *)
  (match p with 
    | King -> not (is_square_in_check dst c b)

    | (Queen | Rook | Bishop) -> (
      (* check that nothing is in the path *)
      match dst_cp with
        (* check for any pieces between second and last square in the path from src to dst *)
        | None -> let piece_found = find_first_piece_between (get_next_square src dst) dst b in
          (match piece_found with 
            | None -> true (* not piece found *)
            | _ -> false)  (* piece found *)

        (* check for any pieces between second and second to last square in the path from src to dst *)
        (* second to last because the last square contains a piece being captured, which is fine *)
        | _ -> let piece_found = find_first_piece_between (get_next_square src dst) (get_next_square dst src) b in
          (match piece_found with
            | None -> true  (* no piece found *)
            | _ -> false)   (* piece found *)
    )
      
    | Pawn -> (
      (* if moving along a rank, the Pawn does not capture anything *)
      if is_move_along_rank src dst then
        (match dst_cp with
          | None -> true  (* does not capture *)
          | _ -> false)   (* captures *)
      
      (* if not moving along a rank, the Pawn does capture a piece *)
      else
        (match dst_cp with
          | None -> false   (* does not capture *)
          | _ -> true)      (* does capture *)
    )

    | Knight -> true
  )

(* TODO: check if is_valid_move dynamically *)
(* for example, disambiguation moves should be checked if they *)
(* are in fact really disambiguation moves *)
let is_valid_move = fun (m : move) (b : Board.board) : bool -> true