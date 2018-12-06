open Abstract_syntax

let analyze_game = fun (g : game) -> 
  let b : Board.board = Board.create() in

  (* static syntax check *)
  if not (Static_syntax_checker.is_valid_game g) then
    raise Exceptions.CompilationException;

  let (m, g) = g;