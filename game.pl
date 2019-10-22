:-include('board.pl').
:-include('list.pl').
:- use_module(library(lists)).



initalizePvsP(Game):-
	emptyBoard(Board),
	cels(Cels),
	firstPlayer(P1),
	Game = [Board, P1, Cels, 1].

getGameBoard([Board|_], Board).

getGamePlayer([_,Player|_], Player).

getGameTypeCels([_,_,Cels,_],Cels).

getGameTurn([_,_,_,Turn],Turn).

nextPlayer(In,Ou):-
    (In = player(1,_) -> secondPlayer(Ou)).

nextPlayer(In,Ou):-
    (In = player(2,_) -> firstPlayer(Ou)).

nextTurn([Board,Player,Cels,Turn], [Board,NextPlayer,Cels, NextTurn]):-
	nextPlayer(Player,NextPlayer),
	NextTurn is Turn + 1.

%%%%%%%%%%%%%% * Section to read user inputs * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call:-
	readInteger(X),write(X),nl,
	readInteger(Y), write(Y),nl .

pressEnter:-
	get_char(_), !.

get_input(Char):-
	get_char(Char),
	pressEnter.


readInput(Input):-
	get_code(Ch),
	readRest(Ch,AllChars),
	name(Input, AllChars).

readRest(10,[]).
readRest(13, []).
readRest(Ch,[Ch|Rest]):-
	get_code(Ch1),
	readRest(Ch1,Rest).

%% qd da mal nao funciona...
readInteger(Int):-
	readInput(Int),
	(integer(Int); write('Is not a integer'),nl, fail),!.

	

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
	RowDest > 0, ColDest > 0,
	getListSize(Board, Size),
	RowDest =< Size, ColDest =< Size.

getSlope(SrcRow, DestRow, SrcCol, DestCol, Slope):-
	Delta_X is SrcRow-DestRow,
	Delta_Y is SrcCol- DestCol,
	Slope is Delta_Y/Delta_X.

getTypePiece(Slope, Answer):- (
		Slope =:= -1 -> Answer = up ;
		Slope =:= 1 -> Answer = down
		).

checkIfSamePoint(SrcRow, DestRow, SrcCol, DestCol):-
	SrcRow = DestRow, SrcCol = DestCol.

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
	 PieceGot = Piece -> true, !
	).


%%%%%%%%%%%%%%%%%%%%%% * Section with menus * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


printMenu:-
	write('================================='), nl,
	write('|          Straigth 4           |'), nl,
	write('|===============================|'), nl,
	write('       Choose an option:        |'), nl,
	write('|                               |'), nl,
	write('|   1. Play                     |'), nl,
	write('|   2. Exit                     |'), nl,
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



menu:-
	printMenu,
	get_input(Char), 
	( Char = '1' -> write('Play the game'),nl, chooseGameMode;
	  Char = '2' -> write('Exiting... the game');
	  nl,
	  write('Please write a valid input'), nl, 
	  pressEnter,nl,
	  menu).


chooseGameMode:-
	printGameMode,
	get_input(Char),
	( Char = '1' -> write('Play the game'),nl, initalizePvsP(Game),gameLoop(Game);
	  Char = '2' -> write('Back to the main menu...'),nl, menu;
	  nl,
	  write('Please write a valid input'), nl, 
	  pressEnter,nl,
	  menu).


	
gameLoop(Game):-
	getGameBoard(Game, Board),
	getGamePlayer(Game, Player),
	getGameTypeCels(Game, Cels),
	getGameTurn(Game, Turn),
	printGameStatus(Turn),
	printBoard(Board,Cels),	
	currentPlayerStatus(Player,Board),
	pressEnter,nl ,
	nextTurn(Game,GameOut),
	gameLoop(GameOut).


game :-
	menu.

	






