(* file on a chess board *)
type file = A | B | C | D | E | F | G | H;;

(* rank on a chess board *)
type rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8;;

(* square on a chess board *)
type square = file * rank;;

(* types of pieces on a chess board *)
type piece = 
  | King
  | Queen
  | Rook
  | Bishop
  | Knight;;

type move = 
  (* simple moves *)
  | PawnMove of square
  | PieceMove of piece * square

  (* disambiguation moves *)
  | DisambiguationUniqueFileMove of file * piece * square
  | DisambiguationUniqueRankMove of rank * piece * square
  | DisambiguationNoUniqueFileOrRankMove of file * rank * piece * square

  (* pawn promotion moves *)
  | PawnMoveAndPromote of square * piece

  (* castling move *)
  | KingSideCastleMove
  | QueenSideCastleMove

  (* capture moves *)
  | CaptureMove of move

  (* move that results to a check *)
  | CheckMove of move
  | DoubleCheckMove of move

  (* checkmate move *)
  | CheckMateMove of move

  (* stale mate move *)
  | StaleMateMove of move

  (* draw move *)
  | DrawMove;;