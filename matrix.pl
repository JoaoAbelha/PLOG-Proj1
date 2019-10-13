create_list(0,_,[]).

create_list(N,Ele,[Ele|Result]):-
    Index is N - 1,
    create_list(Index,Ele, Result).


create_matrix(Nr, Ele, Matrix):-
    create_matrix_aux(Nr,Nr,Ele,Matrix).


create_matrix_aux(0,_,_,[]).
create_matrix_aux(Nr,Size, Ele,[SubList|Rest]):-
    create_list(Size, Ele, SubList),
    Next is Nr - 1,
    create_matrix_aux(Next, Size, Ele, Rest).
    