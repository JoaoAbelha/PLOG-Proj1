:-include('board.pl').

pressEnter:-
	get_char(_), !.

get_input(Char):-
	get_char(Char),
	pressEnter.

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

	






