open Abstract_syntax

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
  (* let dst_p = Hashtbl.find_opt b.square_piece_map s in *)
  (* (match m with
    | Move(p, s) -> match p with
      | Pawn -> 

  ) *)
  true  

let find_src_along_file = fun (c : color) (p : piece) (dst : square) (b : board) (f : file) : square option ->
  let rec recursively_search_upwards = fun (current_rank : rank) : square option -> 
    let cp = Hashtbl.find_opt b.square_piece_map (f, current_rank) in
    let status = 
      (match cp with
      | None -> false
      | Some (current_color, current_piece) -> c == current_color && Utils.is_valid_piece_move (f, current_rank) dst p)
    in

    (* found the piece *)
    if status then Some (f, current_rank)

    (* did not find the piece and reached the last rank *)
    else if current_rank == 7 then None

    (* did not find the piece and can search upwards *)
    else recursively_search_upwards (current_rank + 1)
  in 
  recursively_search_upwards 0

let find_src_along_rank = fun (c : color) (p : piece) (dst : square) (b : board) (r : rank) : square option ->
  let rec recursively_search_rightwards = fun (current_file : file) : square option -> 
    let cp = Hashtbl.find_opt b.square_piece_map (current_file, r) in
    let status = 
      (match cp with
      | None -> false
      | Some(current_color, current_piece) -> current_color == c && Utils.is_valid_piece_move (current_file, r) dst current_piece)
    in

    (* found the piece *)
    if status then Some (current_file, r)

    (* did not find the piece and reached the last rank *)
    else if current_file == 7 then None

    (* did not find the piece and can search upwards *)
    else recursively_search_rightwards (current_file + 1)
  in 
  recursively_search_rightwards 0

(* assumes there can only be one such src, because if not then *)
(* this move would be a disambiguation move and the search space *)
(* would be restricted to either a file or a rank *)
(* searches one rank at a time, starting from rank 0 *)
(* TODO: The search strategy can be optimized based on the piece *)
let find_src_on_the_entire_board = fun (c : color) (p : piece) (dst : square) (b : board) : square option ->
  let rec recursively_search_upwards = fun (current_rank : rank) : square option ->
    let square_found = find_src_along_rank c p dst b current_rank in
    match square_found with
      | None -> 
        if current_rank == 7 then None (* no piece found, and reached the last rank *)
        else recursively_search_upwards (current_rank + 1) (* no piece found, check the next rank *)
      | Some(s) -> square_found (* piece found *)
  in 
  recursively_search_upwards 0

let find_src = fun ?(file_known=false) ?(file=None) ?(rank_known=false) ?(rank=None) (c : color) (p : piece) (dst : square) (b : board) : square option -> 
  (* disambiguation no unique file or rank move *)
  if file_known && rank_known then
    let file = (match file with 
      | Some f -> f
      | None -> raise Exceptions.RunTimeException) (* if file is known, then it needs to be passed in *)
    in
    let rank = (match rank with
      | Some r -> r
      | None -> raise Exceptions.RunTimeException) (* if rank is known, then it needs to be passed in *)
    in 
    Some (file, rank)

  (* disambiguation no unique file move *)
  else if file_known then 
    let file = (match file with 
      | Some f -> f
      | None -> raise Exceptions.RunTimeException) (* if file is known, then it needs to be passed in *)
    in  
    find_src_along_file c p dst b file

  (* disambiguation no unique rank move *)
  else if rank_known then 
    let rank = (match rank with
      | Some r -> r
      | None -> raise Exceptions.RunTimeException) (* if rank is known, then it needs to be passed in *)
    in
    find_src_along_rank c p dst b rank

  else find_src_on_the_entire_board c p dst b

(* deconstructs a move and returns (src, dst, colored_piece) tuple *)
(* to be used only to deconstruct the following moves *)
(*1. Move
  2. DisambiguationUniqueFileMove
  3. DisambiguationUniqueRankMove
  4. DisambiguationNoUniqueFileOrRankMove
  *)
(* if used with any other move, throws a RuntimeException *)
let deconstruct_move = fun (m : move) (b : board) ->
  let rec deconstruct_recursively = fun ?(file_known=false) ?(file=None) ?(rank_known=false) ?(rank=None) (m : move) (b : board) ->
    match m with
      | Move(p, s) -> let src = find_src ~file_known:file_known ~file:file ~rank_known:rank_known ~rank:rank b.current_color p s b in
        (match src with 
          | None -> raise Exceptions.RunTimeException
          | Some(src) -> (src, s, (b.current_color, p)))
      | DisambiguationUniqueFileMove(f, m) -> deconstruct_recursively ~file_known:true ~file:(Some(f)) m b
      | DisambiguationUniqueRankMove(r, m) -> deconstruct_recursively ~rank_known:true ~rank:(Some(r)) m b
      | DisambiguationNoUniqueFileOrRankMove(f, r, m) -> deconstruct_recursively ~file_known:true ~file:(Some(f)) ~rank_known:true ~rank:(Some(r)) m b
      | _ -> raise Exceptions.RunTimeException
  in
  deconstruct_recursively m b

let execute_move = fun (b : board) (m : move) ->
  match m with
    | (Move(_, _) | DisambiguationUniqueFileMove(_, _) | DisambiguationUniqueRankMove(_, _) | DisambiguationNoUniqueFileOrRankMove(_, _, _)) -> 
      let (src, dst, cp) = deconstruct_move m b in
        (* move piece from src to dst *)
        Hashtbl.remove b.square_piece_map src;
        Hashtbl.add b.square_piece_map dst cp;
    
    (* TODO: *)