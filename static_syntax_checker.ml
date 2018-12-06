open Abstract_syntax

let is_valid_file_or_rank = fun (f : file) : bool -> 
  match f with
    | (0 | 1 | 2 | 3 | 4 | 5 | 6 | 7) -> true
    | _ -> false

let is_valid_square = fun (s : square): bool -> 
  let (f, r) = s in
    is_valid_file_or_rank f && is_valid_file_or_rank r


(* check if a move was made by moving a pawn *)
let rec is_pawn_move = fun (m : move) : bool -> 
  match m with
    | Move(p, s) -> (match p with 
      | Pawn -> true
      | _ -> false)
    | DisambiguationUniqueFileMove(_, m) -> is_pawn_move m
    | DisambiguationUniqueRankMove(_, m) -> is_pawn_move m
    | DisambiguationNoUniqueFileOrRankMove(_, _, m) -> is_pawn_move m
    | CaptureMove(m) -> is_pawn_move m
    | CheckMove(m) -> is_pawn_move m
    | DoubleCheckMove(m) -> is_pawn_move m
    | CheckMateMove(m) -> is_pawn_move m
    | StaleMateMove(m) -> is_pawn_move m
    | PawnPromotionMove(_, _) -> true
    | (KingSideCastleMove | QueenSideCastleMove | DrawMove) -> false

(* check if a move is valid *)
let rec is_valid_move = fun (m : move) : bool ->
  match m with
    | Move(p, s) -> is_valid_square s

    (* disambiguation moves can not be made with pawns *)
    | DisambiguationUniqueFileMove(f, m) -> is_valid_file_or_rank f && is_valid_move m && not (is_pawn_move m)
    | DisambiguationUniqueRankMove(r, m) -> is_valid_file_or_rank r && is_valid_move m && not (is_pawn_move m)
    | DisambiguationNoUniqueFileOrRankMove(f, r, m) -> is_valid_file_or_rank f && is_valid_file_or_rank r && is_valid_move m && not (is_pawn_move m)
    
    (* pawn can not be promoted to be a pawn *)
    | PawnPromotionMove(s, p) -> is_valid_square s && (match p with 
      | Pawn -> false
      | _ -> true)

    | CaptureMove(m) -> is_valid_move m
    | CheckMove(m) -> is_valid_move m
    | DoubleCheckMove(m) -> is_valid_move m
    | CheckMateMove(m) -> is_valid_move m
    | StaleMateMove(m) -> is_valid_move m
    | (KingSideCastleMove | QueenSideCastleMove | DrawMove) -> true

(* check if the game ends with a valid move *)
let rec is_valid_game = fun (g : game) : bool -> 
  match g with
    | FinalMove(m) -> (match m with 
      | (CheckMateMove(_) | StaleMateMove(_) | DrawMove) -> true
      | _ -> false) && is_valid_move m
    | Game(m, g) -> is_valid_move m && is_valid_game g