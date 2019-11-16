:- ensure_loaded('movement.pl').
:- ensure_loaded('game_over.pl').

board([ [empty,green, empty, empty, empty],
        [green,empty, green, empty, empty],
        [empty,yellow, empty, empty, yellow],
        [yellow,empty, empty, yellow, empty],
        [empty,empty, empty, green, empty]
]).

call:-
    e2stateBoard(X), cels(C),
    %%value(X,green, yellow, V).
    alpha_beta(game_state(X, C, 4, 4), green, yellow, 2, -1000, 1000, Move, BestMove), write(Move), nl.

get_list_value(List,Piece,Value) :-
	(
		append(_,[empty/N,Piece/M | _], List);
		append(_,[Piece/M,empty/N | _], List);
		append(_,[empty/N, Piece/M, empty/X | _], List)
	),
	(
	    3 < N + M,
		3 is M,
		Value is 5;
		(
			3 < N + M;
			nonvar(X), 3 < N + M + X
		),
		2 is M,
		Value is 2
	), !.

get_list_value(_,_,0). 

evaluate_rows([],_,Acc,Acc).
evaluate_rows([Head|Rest], Piece, Value, Acc) :-
    encode(Head, Values),
    get_list_value(Values, Piece, V),
    Acc1 is Acc + V,
    evaluate_rows(Rest, Piece, Value, Acc1).

evaluate_columns([],_,Acc,Acc).
evaluate_columns([Head|Rest], Piece, Value, Acc) :-
    encode(Head, Values),
    get_list_value(Values, Piece, V),
    Acc1 is Acc + V,
    evaluate_columns(Rest, Piece, Value, Acc1).

evaluate_diagonals([],_,Acc,Acc).
evaluate_diagonals([Head|Rest], Piece, Value, Acc) :-
    encode(Head, Values),
    get_list_value(Values, Piece, V),
    Acc1 is Acc + V,
    evaluate_diagonals(Rest, Piece, Value, Acc1).

value(Board, CurrP, _NextP, 100) :- game_over(Board, CurrP).

value(Board, _CurrP, NextP, -100) :- game_over(Board, NextP).

value(Board, CurrP, NextP, Value) :-
    get_columns(Board, Columns),
    get_diagonals(Board, Diagonals),
    evaluate_rows(Board, CurrP, V1, 0),
    evaluate_columns(Columns, CurrP, V2, 0),
    evaluate_diagonals(Diagonals, CurrP, V3, 0),
    evaluate_rows(Board, NextP, V4, 0),
    evaluate_columns(Columns, NextP, V5, 0),
    evaluate_diagonals(Diagonals, NextP, V6, 0),
    Value is V1 + V2 + V3 - V4 - V5 - V6.

alpha_beta(game_state(Board, _Cels, _NFirst, _NSecond), CurrP, NextP, 0, _Alpha, _Beta, _NoMove, Value) :- 
    value(Board, CurrP, NextP, Value), !.
 
alpha_beta(Game, CurrP, NextP, D, Alpha, Beta, Move, Value) :- 
    D > 0, 
    valid_moves(Game, CurrP, Moves),
    Alpha1 is -Beta, % max/min
    Beta1 is -Alpha,
    D1 is D-1, 
    evaluate_and_choose(Game, CurrP, NextP, Moves, D1, Alpha1, Beta1, nil, (Move,Value)).

evaluate_and_choose(_Game, _CurrP, _NextP, [], _D, Alpha, _Beta, Move, (Move,Alpha)).

evaluate_and_choose(game_state(Board, Cels, NFirst, NSecond), CurrP, NextP, [Move-BoardOut|Moves], D, Alpha, Beta, Record, BestMove) :-
    alpha_beta(game_state(BoardOut, Cels, NFirst, NSecond), NextP, CurrP, D, Alpha, Beta, _OtherMove, Value),
    Value1 is -Value,
    cutoff(game_state(Board, Cels, NFirst, NSecond), CurrP, NextP, Move, Value1, D, Alpha, Beta, Moves, Record, BestMove).
 
cutoff(_Game, _CurrP, _NextP, Move, Value, _D, _Alpha, Beta, _Moves, _Record, (Move,Value)) :- 
    Value >= Beta, !.
cutoff(Game, CurrP, NextP, Move, Value, D, Alpha, Beta, Moves, _Record, BestMove) :- 
    Alpha < Value, Value < Beta, !, 
    evaluate_and_choose(Game, CurrP, NextP, Moves, D, Value, Beta, Move, BestMove).
cutoff(Game, CurrP, NextP, _Move, Value, D, Alpha, Beta, Moves, Record, BestMove) :- 
    Value =< Alpha, !,
    evaluate_and_choose(Game, CurrP, NextP, Moves, D, Alpha, Beta, Record, BestMove).