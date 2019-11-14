:-include('game.pl').

board([[empty,  empty,  empty, empty,  empty],
       [green,  empty,  green, empty,  empty],
       [empty,  green, empty, empty,  yellow],
       [green, empty,  empty, yellow, empty],
       [empty,  empty,  empty, empty,  empty]
]).

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

value(Board, Player, 100) :- win(Board, Player).

value(Board, Player, -100) :- 
    nextPlayer(Player, NextPlayer),
    win(Board, NextPlayer).

value(Board, player(_, Piece), Value) :-
    evaluate_rows(Board, Piece, V1, 0),
    get_columns(Board, Columns),
    evaluate_columns(Columns, Piece, V2, 0),
    get_diagonals(Board, Diagonals),
    evaluate_diagonals(Diagonals, Piece, V3, 0),
    Value is V1 + V2 + V3.

alpha_beta(Game,0,_Alpha,_Beta,_NoMove,Value) :- 
    value(Board,Player, Value).
 
alpha_beta(Game,D,Alpha,Beta,Move,Value) :- 
    D > 0, 
    valid_moves(Game, Moves),
    Alpha1 is -Beta, % max/min
    Beta1 is -Alpha,
    D1 is D-1, 
    evaluate_and_choose(Game,Moves,D1,Alpha1,Beta1,nil,(Move,Value)).
 
evaluate_and_choose(Game,[(Move,BoardOut)|Moves],D,Alpha,Beta,Record,BestMove) :-
	getGamePlayer(Game, Player),
    getGameBoard(Game, Board),
    nextPlayer(Player,NextPlayer),
    alpha_beta([NextPlayer, BoardOut, _, _, _],D,Alpha,Beta,_OtherMove,Value),
    Value1 is -Value,
    cutoff(Game,Move,Value1,D,Alpha,Beta,Moves, Record, BestMove).

evaluate_and_choose(_Game,[],_D,Alpha,_Beta,Move,(Move,Alpha)).
 
cutoff(_Game,Move,Value,_D,_Alpha,Beta,_Moves,_Record,(Move,Value)) :- 
    Value >= Beta, !.
cutoff(Game,Move,Value,D,Alpha,Beta,Moves,_Record,BestMove) :- 
    Alpha < Value, Value < Beta, !, 
    evaluate_and_choose(Game,Moves,D,Value,Beta,Move,BestMove).
cutoff(Game,_Move,Value,D,Alpha,Beta,Moves,Record,BestMove) :- 
    Value =< Alpha, !,
    evaluate_and_choose(Game,Moves,D,Alpha,Beta,Record,BestMove).