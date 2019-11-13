value(Board, Player, 100) :- win(Board, Player).

value(Board, Player, -100) :- 
    nextPlayer(Player, NextPlayer),
    win(Board, NextPlayer).

value(Board, Player, Value) :-

alpha_beta(Player,0,Board,_Alpha,_Beta,_NoMove,Value) :- 
    value(Board,Player, Value).
 
alpha_beta(Player,D,Board,Alpha,Beta,Move,Value) :- 
    D > 0, 
    findall((X,Y),mark(Player,Board,X,Y),Moves), 
    Alpha1 is -Beta, % max/min
    Beta1 is -Alpha,
    D1 is D-1, 
    evaluate_and_choose(Player,Moves,Board,D1,Alpha1,Beta1,nil,(Move,Value)).
 
evaluate_and_choose(Player,[Move|Moves],Board,D,Alpha,Beta,Record,BestMove) :-
    move(Move,Board,BoardOut), 
    nextPlayer(Player,NextPlayer),
    alpha_beta(NextPlayer,D,BoardOut,Alpha,Beta,_OtherMove,Value),
    Value1 is -Value,
    cutoff(Player,Move,Value1,D,Alpha,Beta,Moves,Board,Record,BestMove).

evaluate_and_choose(_Player,[],_Board,_D,Alpha,_Beta,Move,(Move,Alpha)).
 
cutoff(_Player,Move,Value,_D,_Alpha,Beta,_Moves,_Board,_Record,(Move,Value)) :- 
    Value >= Beta, !.
cutoff(Player,Move,Value,D,Alpha,Beta,Moves,Board,_Record,BestMove) :- 
    Alpha < Value, Value < Beta, !, 
    evaluate_and_choose(Player,Moves,Board,D,Value,Beta,Move,BestMove).
cutoff(Player,_Move,Value,D,Alpha,Beta,Moves,Board,Record,BestMove) :- 
    Value =< Alpha, !, ValueValue
    evaluate_and_choose(Player,Moves,Board,D,Alpha,Beta,Record,BestMove).