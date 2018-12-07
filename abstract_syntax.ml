(* file on a chess board *)
type file = int

(* rank on a chess board *)
type rank = int

(* square on a chess board *)
type square = file * rank

(* types of pieces on a chess board *)
type piece = 
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn

type color = White | Black

type colored_piece = color * piece

type move = 
  (* simple moves *)
  | Move of piece * square

  (* disambiguation moves *)
  | DisambiguationUniqueFileMove of file * move
  | DisambiguationUniqueRankMove of rank * move
  | DisambiguationNoUniqueFileOrRankMove of file * rank * move

  (* pawn promotion moves *)
  | PawnPromotionMove of square * piece

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
  | DrawMove

type game = 
  | FinalMove of move
  | Game of move * game

type strategy = 
  | StrategicMove of move * strategy
  | StrategicGame of game
  | IfThenElse of colored_piece * square * strategy * strategy