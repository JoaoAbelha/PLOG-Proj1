:-include('board.pl').
:-include('list.pl').
:- use_module(library(lists)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% * Game class  * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initializePvsP(Game):-
	e2stateBoard(Board),
	cels(Cels),
	firstPlayer(P1), %% should be choosed randomly
	Game = [Board, P1, Cels, 1, pvp].

getGameBoard([Board|_], Board).

getGamePlayer([_,Player|_], Player).

getGameTypeCels([_,_,Cels|_],Cels).

getGameTurn([_,_,_,Turn|_],Turn).

getGameMode([_,_,_,_,Turn], Turn).

nextPlayer(In,Ou):-
    (In = player(1,_) -> secondPlayer(Ou)).

nextPlayer(In,Ou):-
    (In = player(2,_) -> firstPlayer(Ou)).

nextTurn([Board,Player,Cels,Turn,Mode], [Board,NextPlayer,Cels, NextTurn, Mode]):-
	nextPlayer(Player,NextPlayer),
	NextTurn is Turn + 1.

game_mode(pvp).
game_mode(pvb).
game_mode(bvb).

%%%%%%%%%%%%%% * Section to read user inputs * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pressEnter:-
	get_char(_), !.

get_input(Char):-
	get_char(Char),
	get_char(_), !.
get_input(_).


readInput(Input):-
	get_code(Ch),
	readRest(Ch,AllChars),
	name(Input, AllChars).

readRest(10,[]).
readRest(13, []).
readResret(Ch,[Ch|Rest]):-
	get_code(Ch1),
	readRest(Ch1,Rest).


%% por exemplo para input de 1... "1."
read_inteiro(X,Str,Min,Max):-
	repeat,
	format("Coords of ~s: ",[Str]),
	read(X),
	integer(X),
	X >=Min , X =< Max, !.

read_coords(X,Y, Min, Max):-
	read_inteiro(X,"x",Min, Max ),
	read_inteiro(Y,"y",Min, Max),
	write(X),write(Y).

%%%%%%%%%%%%%%%%%%%%%* Validate board   * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% to make sure that the board is empty in the beggining
check_emptyness([],_, _).
check_emptyness([ Row | Tail], Color1, Color2):-
	nth1(_,Row, Color1), nth1(_,Row, Color2),
	check_emptyness(Tail, Color1, Color2).

is_empty(Game):-
	getGameBoard(Board, Game),
	first_player(player(_,Color1)),
	second_player(player(_, Color2)),
	check_emptyness(Board, Color1, Color2).

%% to check the size of the board
check_size([H|T]):-
	getListSize([H|T], Size),
	getListSize(T, Size2),
	Size == 5, Size2 == 5.

	

%%%%%%%%%%%%%%%%%%%%%* Section to validate cel * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call2:-initalizePvsP(Game), %% just to test the ownership function
       getGameBoard(Game, Board),
       getGamePlayer(Game, Player),
       checkOwnerShip(Board,1,1,Player).
	
	

getPiece(Board, Row, Col, Piece):-
	nth0(Row,Board, GetRow),
	nth0(Col,GetRow, Piece).
	

checkOwnerShip(Board,Row, Col, player(_,X)):-
	getPiece(Board, Row, Col, Piece),
	Piece = X.

checkEmpty(Board, Row, Col):-
	getPiece(Board, Row, Col, Piece),
	Piece = empty.

%%%%%%%%%%%%%%%%%%%%%%%% * Section to validate move * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%just to test cal3- check if is a valid movement

call3:-initalizePvsP(Game), 
       isAdjacent(Game, 3, 3, 2, 3).

min(X,Y,Ans):-(
    X < Y ->  Ans is X;
    Ans is Y).

checkOutOfBounds(Board, RowDest, ColDest):-
	RowDest >= 0, ColDest >= 0,
	getListSize(Board, Size),
	RowDest < Size, ColDest < Size.

getSlope(SrcRow, DestRow, SrcCol, DestCol, Slope):-
	Delta_X is SrcRow-DestRow,
	Delta_Y is SrcCol- DestCol,
	Slope is Delta_Y/Delta_X.

getTypePiece(Slope, Answer):- (
		Slope =:= -1 -> Answer = up ;
		Slope =:= 1 -> Answer = down
		).

checkIfSamePoint(SrcRow, DestRow, SrcCol, DestCol):-
	SrcRow =:= DestRow , SrcCol =:= DestCol, !.

checkIfSamePoint(_, _, _, _):-
	format("Not a valid input: Src and Dest coords should be different",[]), nl,
	pressEnter, nl,
	fail.

isAdjacent(Game,SrcRow, DestRow, SrcCol, DestCol):-
	getGameBoard(Game, Board),
	checkOutOfBounds(Board, DestRow, DestCol),
	Delta_X is abs(SrcRow-DestRow),
	Delta_Y is abs(SrcCol- DestCol),
	Delta_X < 2, Delta_Y < 2,
	(Delta_X =:= 0-> true, !;
	 Delta_Y =:= 0 -> true, !;
	 getSlope(SrcRow, DestRow, SrcCol, DestCol,Slope),
	 getTypePiece(Slope,Piece),
	 min(SrcRow,DestRow, MinRow),
	 min(SrcCol,DestCol,MinCol),
	 getGameTypeCels(Game,Cels),
         MiniR is MinRow- 1, MiniC is MinCol-1,
	 getPiece(Cels,MiniR,MiniC,PieceGot),
	 PieceGot = Piece -> true, write("baduh!"), !
	).

isAdjacent(_,_,_,_,_):-
	format("You mande a invalid move",[]), nl,
	format("You can only move a piece to an adjacent vertex of the board!",[]), nl,
	format("Please try again, having that in consideration",[]), nl,
	pressEnter, nl, fail.
	
	

check_cel(Board,X,Y, player(_,Value)):-
	X1 is X + 1,
	Y1 is Y + 1,
	nth1(Y1,Board, Row),
	nth1(X1, Row, Value).

check_cel_empty(Board,X,Y):-
	X1 is X + 1,
	Y1 is Y + 1,
	nth1(Y1,Board, Row),
	nth1(X1, Row, empty).

get_pos(Board, X, Y, Element):-
	X1 is X + 1,
	Y1 is Y + 1,
	nth1(Y1,Board, Row),
	nth1(X1, Row, Element).


%% faz swap do conteudo do tabuleiro
move_piece(SrcRow, DestRow, SrcCol, DestCol, BoardIn, BoardOut):-
	get_pos(BoardIn,SrcRow, SrcCol, Piece), %% retrieves the piece,
	format("~p",[Piece]), nl,
	set_matrix_element_pos(BoardIn, Bout, empty, SrcRow, SrcCol),
	set_matrix_element_pos(Bout, BoardOut, Piece, DestRow, DestCol).
	
	

%%%%%%%%%%%%%%%%%%%%%% * Section with the check final state *%%%%%%%%%%%%%%%%%%%%%%%

call4:-finalStateBoard(Board),
       win(Board, yellow).

%%retorna verdade se List[Index] = Element | Index = indice da primeira ocorrencia
getIndex(List,Element,Index):-
	nth1(Index,List,Element),!.
	

%%checks_for_a_line
checkLine([Row|R], Color):-
	( getIndex(Row, Color, Index),
	  Index2 is Index + 1,
	  getIndex(Row,Color, Index2),
	  Index3 is Index2 + 1,
	  getIndex(Row, Color, Index3),
	  Index4 is Index3 + 1,
	  getIndex(Row, Color, Index4),!
	;
	  checkLine(R,Color)
	).

%%checks_for_a_column...
checkCol([Row, Row2, Row3, Row4|Rest],Color):-
	(getIndex(Row,Color,Index), getIndex(Row2, Color, Index), 
	 getIndex(Row3, Color, Index), getIndex(Row4, Color, Index), !
	;
	 checkCol([Row2, Row3, Row4|Rest], Color)
	).

%%check for diagonals
diagonal_up([Row1, Row2, Row3, Row4|Rest], Color):-
       ( nth1(Index,Row1,Color),
    	 Index2 is Index + 1,
    	 nth1(Index2,Row2,Color),
    	 Index3 is Index2 + 1,
    	 nth1(Index3,Row3,Color),
    	 Index4 is Index3 + 1,
         nth1(Index4,Row4,Color), ! ;
         diagonal_up([Row2, Row3, Row4|Rest], Color)
       ).

diagonal_down([Row1, Row2, Row3, Row4|Rest], Color):-
       ( nth1(Index,Row1,Color),
    	 Index2 is Index - 1,
    	 nth1(Index2,Row2,Color),
    	 Index3 is Index2 - 1,
    	 nth1(Index3,Row3,Color),
    	 Index4 is Index3 - 1,
         nth1(Index4,Row4,Color), ! ;
         diagonal_down([Row2, Row3, Row4|Rest], Color)
       ).
    	

%%still check: only can win after the fourth play (just performance enhancement)
win(Board,player(_,Color)):-
	(  checkLine(Board,Color);
	   checkCol(Board, Color);
	   diagonal_up(Board, Color);
	   diagonal_down(Board, Color)
	).

	

%%%%%%%%%%%%%%%%%%%%%% * Print menus * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


printMenu:-
	write('================================='), nl,
	write('|          Straigth 4           |'), nl,
	write('|===============================|'), nl,
	write('       Choose an option:        |'), nl,
	write('|                               |'), nl,
	write('|   1. Play                     |'), nl,
	write('|   0. Exit                     |'), nl,
	write('|                               |'), nl,
	write('|                               |'), nl,
	write('================================='), nl.

printGameMode:-
	write('================================='), nl,
	write('|          Straigth 4           |'), nl,
	write('|===============================|'), nl,
	write('       Choose an option:        |'), nl,
	write('|                               |'), nl,
	write('|   1. Player Vs Player         |'), nl,
	write('|   2. Back                     |'), nl,
	write('|                               |'), nl,
	write('|                               |'), nl,
	write('================================='), nl.


%%%%%%%%%%%%%%%%%%%% * Menu handling-- eventualmente mudar os read * %%%%%%%%%%%%%%%%%%%%
main_menu:-
	printMenu,
	read(Option),
	integer(Option),
	choose_main_menu(Option).

choose_main_menu(Option):-
	%% Go to the second menu
	Option == 1, write('Play the game'),nl, menu_game_mode;
	%% Exit the game
	Option == 0, write('Exiting the game... Goodbye'),nl, !, true.

choose_main_menu(_):-  main_menu.



menu_game_mode :- 
	printGameMode,
	read(Option),
	integer(Option),
	choose_game_mode_menu(Option).
	
	
choose_game_mode_menu(Option) :-
	%% player vs player
	Option == 1, format("Player vs Player",[]),nl, initializePvsP(Game), game_loop(Game);
	%% Go back to the main menu
	Option == 2, nl, main_menu.

choose_game_mode_menu(_):- menu_game_mode.
%%%%%%%%%%%%%%%%%%%%%% * Human Move * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

human_play(Game, GameOut):-
	getGameBoard(Game, Board),
	getGamePlayer(Game, Player),
	getGameTypeCels(Game, Cels),
	getGameTurn(Game, Turn),
	repeat,
	printGameStatus(Turn),
	printBoard(Board,Cels),	
	currentPlayerStatus(Player,Board),
	pressEnter,nl ,
	(
	%% before all the pieces are in the board
	%%Turn =< 8,
	%%read_coords(X, Y, 0, 4),
	%%format("read coords",[]),nl,
	%%check_cel_empty(Board, X, Y),
	%%format("check cel empty",[]),nl,
	%%Player = player(_,Color),
	%%format("~p",[Color]),nl,
	%%set_matrix_element_pos(Board, BoardOut, Color, Y, X),
	%%format("set matrix",[]),nl,
	%%nextTurn([BoardOut, Player, Cels, Turn, pvp], GameOut), nl, format("next turn",[]), nl ,!
	%%;
	%%Turn > 8,
	%% after all the pieces in the board
	read_coords(X1,Y1, 0, 4), %%choose coords where a piece is
	check_cel(Board, X1, Y1, Player),
	format("~s",["1..."]), nl,
	read_coords(X2, Y2, 0, 4), %% where should this be moved
	\+ checkIfSamePoint(Y1, Y2, X1, X2),
	format("~s",["2..."]), nl,
	isAdjacent(Game,Y1, Y2, X1, X2),
	format("~s",["3..."]), nl,
	move_piece(X1, X2, Y1, Y2, Board, BoardOut),
	format("~s",["4..."]), nl,
	nextTurn([BoardOut,Player, Cels,Turn, pvp] ,GameOut), !
       ).
	
	
	
%%%%%%%%%%%%%%%%%%%%%% Game Loop %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


game :-
	main_menu.

game_loop(Game):-
	getGamePlayer(Game,Player),
	getGameBoard(Game,Board),
	\+ win(Board,Player),
	(	
	getGameMode(Game,Mode),
	%% cut needed bc there will be other options
	Mode == pvp, human_play(Game,GameOut), game_loop(GameOut), ! 
	
	).
game_loop(_):-format("Someone Won",[]), nl, main_menu.
		
	
	
