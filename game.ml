open Abstract_syntax

let rec static_check_all_moves = fun (g : game) : bool -> 
  match g with
    | FinalMove(m) -> Static_syntax_checker.is_valid_move m
    | Game(m, g) -> Static_syntax_checker.is_valid_move m && static_check_all_moves g

let execute_move = fun (m : move) (b : Board.board) ->
  match m with
    | (Move(_, _) | DisambiguationUniqueFileMove(_, _) | DisambiguationUniqueRankMove(_, _) | DisambiguationNoUniqueFileOrRankMove(_, _, _)) -> 
      let (src, dst, cp) = Board.deconstruct_move m b in
        if not (Semantics_checker.is_valid_game_move src dst cp b) then
          raise Exceptions.SemanticsError
        else 
          Board.execute_move b m
    | _ -> Board.execute_move b m

(* start execution of the game *)
let rec execute_game = fun (g : game) (b : Board.board) -> 
  match g with
    | FinalMove(m) -> 
      if Semantics_checker.is_valid_move m b then
        (* make move *)
        Board.execute_move b m;
        None
    | Game(m, g) -> 
      if Semantics_checker.is_valid_move m b then
        (* make move *)
        Board.execute_move b m;
        execute_game g b;
        None

let analyze_game = fun (g : game) -> 
  let b : Board.board = Board.create() in

  (* static syntax check *)
  if not (Static_syntax_checker.is_valid_game g) then
    raise Exceptions.SyntaxError
  else if not (static_check_all_moves g) then
    raise Exceptions.SyntaxError

  else execute_game g b