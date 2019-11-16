:-use_module(library(lists)).
:-use_module(library(random)).
:- ensure_loaded('movement.pl').
:- ensure_loaded('player.pl').

initial_game_state(game_state(Board,Cels,NFirst,NSecond)) :-
	emptyBoard(Board),
	cels(Cels),
	getNrPieces(Board, green, NFirst),
	getNrPieces(Board, yellow, NSecond).

nextTurn(game_state(_, Cels, _, _), game_state(BoardOut, Cels, NFirst2, NSecond2), BoardOut) :-
	getNrPieces(BoardOut, green, NFirst2),
	getNrPieces(BoardOut, yellow, NSecond2).

%%checks_for_a_line
checkLine([Row|R], Piece):-
	( 
		nth0(Index, Row, Piece),
	  	Index2 is Index + 1,
	  	nth0(Index2, Row, Piece),
	  	Index3 is Index2 + 1,
	  	nth0(Index3, Row, Piece),
	  	Index4 is Index3 + 1,
	  	nth0(Index4, Row, Piece), !;
	  	checkLine(R, Piece)
	).

%%checks_for_a_column
checkCol([Row, Row2, Row3, Row4|Rest], Piece):-
	(
		nth0(Index, Row, Piece),
		nth0(Index, Row2, Piece),
		nth0(Index, Row3, Piece),
		nth0(Index, Row4, Piece), !;
	 	checkCol([Row2, Row3, Row4|Rest], Piece)
	).

%%check for diagonals
diagonal_up([Row1, Row2, Row3, Row4|Rest], Piece):-
	( 
		nth0(Index, Row1, Piece),
    	Index2 is Index + 1,
		nth0(Index2, Row2, Piece),
    	Index3 is Index2 + 1,
    	nth0(Index3, Row3, Piece),
    	Index4 is Index3 + 1,
        nth0(Index4, Row4, Piece), !;
        diagonal_up([Row2, Row3, Row4|Rest], Piece)
    ).

diagonal_down([Row1, Row2, Row3, Row4|Rest], Piece):-
    ( 
		nth0(Index, Row1, Piece),
    	Index2 is Index - 1,
    	nth0(Index2, Row2, Piece),
    	Index3 is Index2 - 1,
    	nth0(Index3, Row3, Piece),
    	Index4 is Index3 - 1,
        nth0(Index4, Row4, Piece), ! ;
        diagonal_down([Row2, Row3, Row4|Rest], Piece)
    ).	

%%still check: only can win after the fourth play (just performance enhancement)
win(Board, player(Piece,_)) :-
	(  
		checkLine(Board, Piece);
		checkCol(Board, Piece);
		diagonal_up(Board, Piece);
		diagonal_down(Board, Piece)
	).

%%%%%%%%%%%%%%%%%%%%%% * Print menus * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_menu :-
	write('================================='), nl,
	write('|          Straigth 4           |'), nl,
	write('|===============================|'), nl,
	write('       Choose an option:        |'), nl,
	write('|                               |'), nl,
	write('|   1. Play                     |'), nl,
	write('|   2. Credits                  |'), nl,
	write('|   0. Exit                     |'), nl,
	write('|                               |'), nl,
	write('================================='), nl.

print_credits :-
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

%%%%%%%%%%%%%%%%%%%% * Menu handling-- eventualmente mudar os read * %%%%%%%%%%%%%%%%%%%%

main_menu:-
	print_menu,
	catch(read(Option), _, fail),
	integer(Option),
	choose_main_menu(Option).

choose_main_menu(Option):-
	Option == 1, write('Play the game'), nl, !;
	Option == 0, write('Exiting the game... Goodbye'), nl, !, fail;
	Option == 2, nl, credits_menu.
	
choose_main_menu(_):-  main_menu.

credits_menu:-
	print_credits,
	catch(read(Option), _, fail),
	integer(Option),
	Option =:= 0, main_menu;
	credits_menu.

%%%%%%%%%%%%%%%%%%%%%% * Human Move * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_game :-
	now(TS),
    setrand(TS),
	initial_game_state(Game),
	main_menu,
	initial_player(P1),
	second_player(P2),
	play_game(Game, P1, P2, 1).

display_winner(player(Piece, _)) :- 
    write('Player '), write(Piece), write(' won!'), nl, fail.

display_invalid_move :-
	format("You made a invalid move",[]), nl,
	format("You can only move a piece to an adjacent vertex of the board!",[]), nl,
	format("Please try again, having that in consideration",[]), nl,
	pressEnter, pressEnter, nl.

play_game(game_state(Board, Cels, _, _), CurrP, NextP, NTurns) :-
	printGameStatus(NTurns),
	printBoard(Board,Cels),
	currentPlayerStatus(CurrP,Board), nl,
	(
		win(Board, CurrP), !, display_winner(CurrP);
		win(Board, NextP), !, display_winner(NextP)	
	).

play_game(GameState, player(CurrP, CurrType), player(NextP, NextType), NTurns) :-
    get_move(CurrType, GameState, Move, CurrP, NextP, NTurns),
    move(GameState, CurrP, Move, BoardOut), !,
    NTurns2 is NTurns + 1,
	nextTurn(GameState, NewGameState, BoardOut),
    play_game(NewGameState, player(NextP, NextType), player(CurrP, CurrType), NTurns2).

play_game(GameState, CurrP, NextP, NTurns) :-
    display_invalid_move, !, play_game(GameState, CurrP, NextP, NTurns).