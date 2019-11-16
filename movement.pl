:- ensure_loaded('board.pl').

valid_move(Board, move(Col, Row)) :-
	insideBoard(Board, Col, Row),
	get_element(Board, empty, Col, Row).

valid_move(Board, Cels, move(SrcCol, SrcRow, DestCol, DestRow), Piece) :-
	insideBoard(Board, SrcCol, SrcRow),
	insideBoard(Board, DestCol, DestRow),
	get_element(Board, empty, DestCol, DestRow),
	get_element(Board, Piece, SrcCol, SrcRow),
	is_adjacent(move(SrcCol, SrcRow, DestCol, DestRow), Cels).

is_adjacent(move(SrcCol, SrcRow, DestCol, DestRow), Cels) :-
	is_diagonal(move(SrcCol, SrcRow, DestCol, DestRow)), !,
	Delta_X is abs(SrcCol - DestCol),
	Delta_Y is abs(SrcRow - DestRow),
	Delta_X < 2, Delta_Y < 2,
	get_slope(move(SrcCol, SrcRow, DestCol, DestRow), Slope),
	get_cel_type(Slope, Piece),
	check_cel(move(SrcCol, SrcRow, DestCol, DestRow), Cels, Piece).

is_adjacent(move(SrcCol, SrcRow, DestCol, DestRow), _) :-
	Delta_X is abs(SrcCol - DestCol),
	Delta_Y is abs(SrcRow - DestRow),
	Delta_X < 2, Delta_Y < 2.

is_diagonal(move(SrcCol, SrcRow, DestCol, DestRow)) :-
	AbsDif_X is abs(DestCol - SrcCol),
    AbsDif_Y is abs(DestRow - SrcRow),
    AbsDif_X =:= AbsDif_Y.

get_slope(move(SrcCol, SrcRow, DestCol, DestRow), Slope):-
	Delta_X is SrcCol - DestCol,
	Delta_Y is SrcRow - DestRow,
	Slope is Delta_X/Delta_Y.

get_cel_type(-1.0, up).
get_cel_type(1.0, down).

check_cel(move(SrcCol, SrcRow, DestCol, DestRow), Cels, Piece) :-
    MinRow is min(SrcRow, DestRow), 
	MinCol is min(SrcCol, DestCol),
	get_element(Cels, PieceGot, MinCol, MinRow),
	PieceGot = Piece.

move_piece(move(SrcCol, SrcRow, DestCol, DestRow), Piece, BoardIn, BoardOut) :- !,
	set_matrix_element_pos(BoardIn, Bout, empty, SrcCol, SrcRow),
	set_matrix_element_pos(Bout, BoardOut, Piece, DestCol, DestRow).

move_piece(move(Col, Row), Piece, BoardIn, BoardOut) :- !,
	set_matrix_element_pos(BoardIn, BoardOut, Piece, Col, Row).
	
move(game_state(Board, Cels, 4, 4), Piece, Move, BoardOut) :- !,
	valid_move(Board, Cels, Move, Piece),
	move_piece(Move, Piece, Board, BoardOut).

move(game_state(Board, _, _, _), Piece, Move, BoardOut) :- !,
	valid_move(Board, Move),
	move_piece(Move, Piece, Board, BoardOut).

valid_moves(Game, player(Piece, _), ListOfMoves) :-
	findall((Move,BoardOut), move(Game, Piece, Move, BoardOut), ListOfMoves).