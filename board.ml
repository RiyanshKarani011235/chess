open Abstract_syntax

(* module SquarePieceMap = struct
  type t = {
    square_piece_map : (square, colored_piece) Hashtbl.t;
    piece_square_map : (colored_piece, square) Hashtbl.t;
  }

  let clear = fun (t : t) -> (
    Hashtbl.clear t.square_piece_map;
    Hashtbl.clear t.piece_square_map;
  )

  let add_piece = fun (t : t) (cp : colored_piece) (s : square) -> (
    Hashtbl.add t.square_piece_map s cp;
    Hashtbl.add t.piece_square_map cp s;
  )
end *)

type castle_status = {
  queen_side_castle_legal : bool;
  king_side_castle_legal : bool;
}

type board = {
  white_castle_status : castle_status;
  black_castle_status : castle_status;
  square_piece_map : (square, colored_piece) Hashtbl.t;
  current_color : color;
}

let create_initial_castle_status = fun () : castle_status -> {
  queen_side_castle_legal = true;
  king_side_castle_legal = true;
}

let create_initial_square_piece_map = fun () : ((square, colored_piece) Hashtbl.t) ->
  let square_piece_map : (square, colored_piece) Hashtbl.t = Hashtbl.create 64 in

  (* white pawns *)
  for i = 0 to 7 do
    Hashtbl.add square_piece_map (i, 1) (White, Pawn)
  done;

  (* black pawns *)
  for i = 0 to 7 do
    Hashtbl.add square_piece_map (i, 6) (White, Pawn)
  done;

  (* white pieces *)
  Hashtbl.add square_piece_map (0, 0) (White, Rook);
  Hashtbl.add square_piece_map (1, 0) (White, Knight);
  Hashtbl.add square_piece_map (2, 0) (White, Bishop);
  Hashtbl.add square_piece_map (3, 0) (White, Queen);
  Hashtbl.add square_piece_map (4, 0) (White, King);
  Hashtbl.add square_piece_map (5, 0) (White, Bishop);
  Hashtbl.add square_piece_map (6, 0) (White, Knight);
  Hashtbl.add square_piece_map (7, 0) (White, Bishop);
  
  (* Black pieces *)
  Hashtbl.add square_piece_map (0, 7) (White, Rook);
  Hashtbl.add square_piece_map (1, 7) (White, Knight);
  Hashtbl.add square_piece_map (2, 7) (White, Bishop);
  Hashtbl.add square_piece_map (3, 7) (White, Queen);
  Hashtbl.add square_piece_map (4, 7) (White, King);
  Hashtbl.add square_piece_map (5, 7) (White, Bishop);
  Hashtbl.add square_piece_map (6, 7) (White, Knight);
  Hashtbl.add square_piece_map (7, 7) (White, Bishop);

  square_piece_map

let create = fun () : board -> {
  white_castle_status = create_initial_castle_status();
  black_castle_status = create_initial_castle_status();
  square_piece_map = create_initial_square_piece_map();
  current_color = White;
}

let make_move = fun (b : board) (m : move) ->
  (match m with
    | Move(p, s) -> true
  )
    