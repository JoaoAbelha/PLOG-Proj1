:-include('board.pl').
:-include('list.pl').
:- use_module(library(lists)).
:-use_module(library(random)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%% * Game class  * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initializePvsP(Game):-
	e2stateBoard(Board),
	cels(Cels),
	firstPlayer(P1), %% should be choosed randomly
	Game = [Board, P1, Cels, 1, pvp].


initializePvsRandom(Game):-
	emptyBoard(Board),
	cels(Cels),
	firstPlayer(P1), %% should be choosed randomly
	Game = [Board, P1, Cels, 1, pvb].

initializebvsb(Game):-
	emptyBoard(Board),
	cels(Cels),
	firstPlayer(P1), %% should be choosed randomly
	Game = [Board, P1, Cels, 1, bvb].


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

call3(List) :-
	initializePvsP(Game), 
    valid_moves(Game,List).

check_same_point(move(SrcCol, DestCol, SrcRow, DestRow)) :-
	DestRow is SrcRow, 
	DestCol is SrcCol.

%%%%%%%%%%%%%% Move %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
valid_move(Game, move(SrcCol, DestCol, SrcRow, DestRow)) :-
	getGameBoard(Game, Board),
	getGamePlayer(Game, Player),
	getGameTypeCels(Game, Cels),
	Player = player(_, Piece),
	insideBoard(Board, SrcCol, SrcRow),
	insideBoard(Board, DestCol, DestRow),
	get_element(Board, empty, DestCol, DestRow),
	get_element(Board, Piece, SrcCol, SrcRow),
	is_adjacent(move(SrcCol, DestCol, SrcRow, DestRow), Cels),
	format("Src - (~d,~d), Dest - (~d,~d)",[SrcCol,SrcRow,DestCol,DestRow]), nl.

is_adjacent(move(SrcCol, DestCol, SrcRow, DestRow), Cels) :-
	is_diagonal(move(SrcCol, DestCol, SrcRow, DestRow)), !,
	Delta_X is abs(SrcCol - DestCol),
	Delta_Y is abs(SrcRow - DestRow),
	Delta_X < 2, Delta_Y < 2,
	get_slope(move(SrcCol, DestCol, SrcRow, DestRow), Slope),
	get_cel_type(Slope, Piece),
	check_cel(move(SrcCol, DestCol, SrcRow, DestRow), Cels, Piece).

is_adjacent(move(SrcCol, DestCol, SrcRow, DestRow), _) :-
	Delta_X is abs(SrcCol - DestCol),
	Delta_Y is abs(SrcRow - DestRow),
	Delta_X < 2, Delta_Y < 2.

is_diagonal(move(SrcCol, DestCol, SrcRow, DestRow)) :-
	AbsDif_X is abs(DestCol - SrcCol),
    AbsDif_Y is abs(DestRow - SrcRow),
    AbsDif_X =:= AbsDif_Y.

get_slope(move(SrcCol, DestCol, SrcRow, DestRow), Slope):-
	Delta_X is SrcCol - DestCol,
	Delta_Y is SrcRow - DestRow,
	Slope is Delta_X/Delta_Y.

get_cel_type(-1.0, up).
get_cel_type(1.0, down).

check_cel(move(SrcCol, DestCol, SrcRow, DestRow), Cels, Piece) :-
    MinRow is min(SrcRow, DestRow), 
	MinCol is min(SrcCol, DestCol),
	getPiece(Cels, MinRow, MinCol, PieceGot),
	PieceGot = Piece.
	
move(move(SrcCol, DestCol, SrcRow, DestRow), Game) :-
	%%getGameBoard(Game, Board),
	valid_move(Game, move(SrcCol, DestCol, SrcRow, DestRow)).
	%%move_piece(Move, Board, BoardOut).

valid_moves(Game, ListOfMoves) :-
	findall(Move, move(Move, Game), ListOfMoves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isAdjacent(_,_,_,_,_):-
	format("You mande a invalid move",[]), nl,
	format("You can only move a piece to an adjacent vertex of the board!",[]), nl,
	format("Please try again, having that in consideration",[]), nl,
	pressEnter, nl, fail.
	
	

check_cel(Board, X, Y, player(_,Value)):-
	nth0(Y,Board, Row),
	nth0(X, Row, Value).

check_cel_empty(Board,X,Y):-
	nth0(Y,Board, Row),
	nth0(X, Row, empty).

get_pos(Board, X, Y, Element):-
	nth0(Y,Board, Row),
	nth0(X, Row, Element).


%% faz swap do conteudo do tabuleiro
move_piece(move(SrcRow, DestRow, SrcCol, DestCol), BoardIn, BoardOut):-
	get_pos(BoardIn,SrcRow, SrcCol, Piece), %% retrieves the piece,
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


getDiagonal([Row1, Row2, Row3, Row4|Rest], [[Color1,Color2,Color3,Color4]|List]):-
       ( nth0(Index1,Row1,Color1),
    	 Index2 is Index1 + 1,
    	 nth0(Index2,Row2,Color2),
    	 Index3 is Index2 + 1,
    	 nth0(Index3,Row3,Color3),
    	 Index4 is Index3 + 1,
         nth0(Index4,Row4, Color4);
         diagonal_up([Row2, Row3, Row4|Rest], List)
       ).

getDiagonal(_,[]).

call4(Result):-
	initializePvsP(Game),
	getGameBoard(Board, Game),
	findall(X, getDiagonal(Board, X	), Result).
	
    	

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
	write('|   2. Credits                  |'), nl,
	write('|   0. Exit                     |'), nl,
	write('|                               |'), nl,
	write('|                               |'), nl,
	write('================================='), nl.

printCredits:-
	write('================================='), nl,
	write('|          Straigth 4           |'), nl,
	write('|===============================|'), nl,
	write('|           Credits:            |'), nl,
	write('|    This beautiful game        |'), nl,
	write('|       was made by:            |'), nl,
	write('|                               |'), nl,
	write('|   * JOAO ABELHA               |'), nl,
	write('|   * VITOR BARBOSA             |'), nl,
	write('|                               |'), nl,
	write('|   0 - Back                    |'), nl,
	write('================================='), nl.

printGameMode:-
	write('================================='), nl,
	write('|          Straigth 4           |'), nl,
	write('|===============================|'), nl,
	write('|      Choose an option:        |'), nl,
	write('|                               |'), nl,
	write('|   1. Player Vs Player         |'), nl,
	write('|   2. Player Vs Bot            |'), nl,
	write('|   3. Bot Vs Bot               |'), nl,
	write('|   0. Back                     |'), nl,
	write('|                               |'), nl,
	write('|                               |'), nl,
	write('================================='), nl.

printPVsB:-
	write('================================='), nl,
	write('|          Straigth 4           |'), nl,
	write('|===============================|'), nl,
	write('|      Choose the bot you       |'), nl,
	write('|     want to play against:     |'), nl,
	write('|                               |'), nl,
	write('|   1. Random                   |'), nl,
	write('|   2. Smart                    |'), nl,
	write('|   0. Back                     |'), nl,
	write('|                               |'), nl,
	write('|                               |'), nl,
	write('================================='), nl.

printBVsB:-
	write('================================='), nl,
	write('|          Straigth 4           |'), nl,
	write('|===============================|'), nl,
	write('|      Choose the bots you      |'), nl,
	write('|     want to see play:         |'), nl,
	write('|                               |'), nl,
	write('|   1. Random vs Random         |'), nl,
	write('|   2. Random vs Smart          |'), nl,
	write('|   2. Smart vs Smart           |'), nl,
	write('|   0. Back                     |'), nl,
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
	Option == 1, write('Play the game'),nl, menu_game_mode, !;
	%% Exit the game
	Option == 0, write('Exiting the game... Goodbye'),nl, !, true ;
	%% check the credits
	Option == 2, nl, credits_menu.
	
choose_main_menu(_):-  main_menu.

credits_menu:-
	repeat,
	printCredits,
	read(Option),
	integer(Option),
	Option =:= 0, main_menu.



menu_game_mode :- 
	printGameMode,
	read(Option),
	integer(Option),
	choose_game_mode_menu(Option).

menu_bot_mode :-
	repeat,
	printPVsB,
	read(Option),
	integer(Option),
	(Option == 0, menu_game_mode, !; 
	 Option == 1, initializePvsRandom(Game), game_loop(Game), !

	).
	
menu_bot_vs_bot :-
	repeat,
	printBVsB,
	read(Option),
	integer(Option),
	(Option == 0, menu_game_mode, !;
	 Option == 1, initializebvsb(Game), game_loop(Game), ! ).
	%% faltam os restantes bots
	
	
	
choose_game_mode_menu(Option) :-
	%% player vs player
	Option == 1, format("Player vs Player",[]),nl, initializePvsP(Game), game_loop(Game), !;
	%% player vs bot
	Option == 2, format("Player vs Bot", []), nl, menu_bot_mode, !;
	%%bot vs bot
	Option == 3, menu_bot_vs_bot,!;
	%% Go back to the main menu
	Option == 0, nl, main_menu.

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
	Turn =< 8,
	read_coords(X, Y, 0, 4),
	format("read coords",[]),nl,
	check_cel_empty(Board, X, Y),
	format("check cel empty",[]),nl,
	Player = player(_,Color),
	format("~p",[Color]),nl,
	set_matrix_element_pos(Board, BoardOut, Color, Y, X),
	format("set matrix",[]),nl,
	nextTurn([BoardOut, Player, Cels, Turn, pvp], GameOut), nl, format("next turn",[]), nl ,!
	;
	%%Turn > 8,
	%% after all the pieces in the board
	read_coords(X1,Y1, 0, 4), %%choose coords where a piece is
	check_cel(Board, X1, Y1, Player),
	format("~s",["1..."]), nl,
	read_coords(X2, Y2, 0, 4), %% where should this be move
	format("~s",["2..."]), nl,
	isAdjacent(Game,Y1, Y2, X1, X2),
	format("~s",["3..."]), nl,
	move_piece(X1, X2, Y1, Y2, Board, BoardOut),
	format("~s",["4..."]), nl,
	nextTurn([BoardOut,Player, Cels,Turn, pvp] ,GameOut), !
       ).

%%%%%%%%%%%%%%%%%%%%%%%% human vs bot random %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%since its a small board there is not a big impact at doing this
random_find(Board,Color,Y, X):-
    repeat,
    random(0, 4, Y),
    nth0(Y, Board, Col),
    random(0,4 ,X),
    nth0(X, Col, Color).



bot_play(GameIn, GameOut):-
	getGameBoard(GameIn, Board),
	getGamePlayer(GameIn, Player),
	getGameTypeCels(GameIn, Cels),
	getGameTurn(GameIn, Turn),
	printGameStatus(Turn),
	printBoard(Board,Cels),	
	currentPlayerStatus(Player,Board),
	pressEnter,nl ,
	repeat,
	(
	Turn =< 8,
	random(0, 4, Row), random(0, 4, Col),
	checkEmpty(Board, Row, Col), %% should be empty
	format("checked empty",[]),nl,
	Player = player(_,Color), %% bot is a player
	set_matrix_element_pos(Board, BoardOut, Color, Col, Row),
	format("set matrix",[]),nl,
	nextTurn([BoardOut, Player, Cels, Turn, bvb], GameOut), nl ,!;
	Turn > 8,
	Player = player(_,Color), %% bot is a player
	format("b1",[]),
	random_find(Board, Color, Row, Col), %%choose random position
	format("b2",[]),
	%% function to calculate random adjacent free cel TODO
	format("b3",[]),
	move_piece(Col, DestCol, Row, DestRow, Board, BoardOut),
	format("b4",[]),
	nextTurn([BoardOut,Player, Cels,Turn, bvb] ,GameOut)
	).
	


	
	
	
	
%%%%%%%%%%%%%%%%%%%%%% Game Loop %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


game :-
	main_menu.

game_loop(Game):-
	getGamePlayer(Game,Player),
	getGameMode(Game,Mode),
	(	
	Mode == pvp, human_play(Game,GameOut),getGameBoard(GameOut,NewBoard),
	\+win(NewBoard,Player), game_loop(GameOut), !;

	Mode == pvb, 
	getGameBoard(GameOut,NewBoard),	
	human_play(Game,GameOut),\+win(NewBoard,Player),
	bot_play(GameOut, GameOut2),\+win(NewBoard,Player),game_loop(GameOut2), ! ;
	
	Mode == bvb, bot_play(Game,GameOut),getGameBoard(GameOut,NewBoard),
	\+win(NewBoard,Player), game_loop(GameOut), !
	).


game_loop(Game):- getGamePlayer(Game,player(_,Piece)),
		  format("Player ~s won!",[Piece]), nl, 
	          main_menu. %% back to the main menu
		
	
	
