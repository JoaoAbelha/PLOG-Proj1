:-use_module(library(lists)).

isEmpty([]).

%%calculate the size of the list using accumulators
getListSize(List, Size):-
	listSize(List, 0, Size).
listSize([],Size,Size).
listSize([_|Tail],Size, Result):-
	NewSize is Size + 1,
	listSize(Tail,NewSize,Result).

%% calculate number of elements using accumulators
getNrElements(List,Element,Nr):-
	getNr_aux(List, Element, 0 , Nr).

getNr_aux([],_,Nr,Nr).
getNr_aux([Head|Tail], Head, Nr, Result):-
	NrAtual is Nr + 1,
	getNr_aux(Tail, Head, NrAtual, Result).
getNr_aux([_|Tail], Ele, Nr, Result):-
	getNr_aux(Tail, Ele, Nr, Result).

%%list of lists calculate the number of elements of this
getBoardNrElements(Board, Number, Element):-
	getBoardNr_aux(Board, Element, 0, Number).

getBoardNr_aux([], _, Nr, Nr). 
getBoardNr_aux([List|Rest], Element, Number, Result):-
	getNrElements(List, Element, GetNumber),
	AtualCount is Number + GetNumber,
	getBoardNr_aux(Rest, Element, AtualCount, Result).

set_list_element([_|T],[Element|T],Element,0).
set_list_element([Head|T1],[Head|List],Element,X):-
	X > 0, !,
	Xf is X - 1,
	set_list_element(T1,List,Element,Xf).

set_matrix_element_pos([RowIn|RestIn],[RowOut|RestIn],Element,X,0):-
	set_list_element(RowIn, RowOut, Element, X).

set_matrix_element_pos([RowIn|RestIn], [RowIn|RestOut], Element, X, Y):-
	Y > 0, !,
	Yf is Y - 1,
	set_matrix_element_pos(RestIn,RestOut, Element, X, Yf).

get_element(Board, Element, X, Y) :-
	nth0(Y, Board, BoardRow),
    nth0(X, BoardRow, Element).

pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- X \= Y.
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).

encode(L1,L2) :- pack(L1,L), transform(L,L2).

transform([],[]).
transform([[X|Xs]|Ys],[X/N|Zs]) :- length([X|Xs],N), transform(Ys,Zs).

get_list_value(List,Piece,Value) :-
	(
		append(_,[empty/N,Piece/M | _], List);
		append(_,[Piece/M,empty/N | _], List);
		append(_,[empty/N, Piece/M, empty/X | _], List)
	),
	(
		(
			3 < N+M;
			nonvar(X), 3 < N+M+X
		),
		3 is M,
		Value is 5;
		(
			3 < N+M;
			nonvar(X), 3 < N+M+X
		),
		2 is M,
		Value is 2
	).

get_list_value(_,_,0).

get_columns([[]|_], []).
get_columns(Matrix, [Column|Rest]) :-
	column(Matrix, Column, Opa),
	get_columns(Opa, Rest).

column([], [], []).
column([[S|ColumnRest]|Rest], [S|Column], [ColumnRest|RowsRest]) :-
	column(Rest, Column, RowsRest).

add_char(0,[]).
add_char(N, [$|Rest]) :-
  	N > 0,
  	N1 is N - 1,
  	add_char(N1, Rest).

remove_char([], []).
remove_char([Row|RowsRest], [Diagonal|DiagonalsRest]) :-
 	delete(Row, $, Diagonal),
  	remove_char(RowsRest, DiagonalsRest).

get_diagonals_up(Matrix, Size, DiagonalsUp) :-
  	N is Size - 1,
  	get_diagonals_up_aux(Matrix, 0, N, Diagonals),
  	get_columns(Diagonals, Columns),
  	remove_char(Columns, DiagonalsUp).

get_diagonals_up_aux([], _, -1, []).
get_diagonals_up_aux([Head|Tail], Left, Rigth, [DiagonalsUp|Rest]) :-
  	Rigth >= 0,
  	LeftN is Left + 1,
  	RigthN is Rigth - 1,
  	add_char(Left, ListLeft),
  	add_char(Rigth, ListRigth),
  	append(ListLeft, Head, BoardLeft),
	append(BoardLeft, ListRigth, DiagonalsUp),
	get_diagonals_up_aux(Tail, LeftN, RigthN, Rest).

get_diagonals_down_aux([], -1, _, []).
get_diagonals_down_aux([Head|Tail], Left, Rigth, [DiagonalsDown|Rest]) :-
	Left >= 0,
	LeftN is Left - 1,
	RigthN is Rigth + 1,
	add_char(Left, ListLeft),
	add_char(Rigth, ListRigth),
	append(ListLeft, Head, BoardLeft),
	append(BoardLeft, ListRigth, DiagonalsDown),
	get_diagonals_down_aux(Tail, LeftN, RigthN, Rest).

get_diagonals_down(Board, Size, DiagonalsDown) :-
	N is Size - 1,
	get_diagonals_down_aux(Board, N, 0, Diagonals),
	get_columns(Diagonals, Columns),
	remove_char(Columns, DiagonalsDown).

remove_small_diagonals(Size, Diagonals, Ans) :-
remove_small_diagonals(Size, Diagonals, Ans, []), !.
remove_small_diagonals( _, [], Ans, Ans).
remove_small_diagonals( Size, [Diagonal | Rest], Ans, Acc ) :-
	length(Diagonal, N),
    (
        ( N < Size, NewEl = [] )
        ; 
         NewEl = [Diagonal]
    ),    
    append( Acc, NewEl, NewAcc ),
    remove_small_diagonals( Size, Rest, Ans, NewAcc).

get_diagonals(Board, Diagonals) :-
	length(Board, N),
	get_diagonals_down(Board, N, DiagonalsDown),
	get_diagonals_up(Board,N, DiagonalsUp),
	append(DiagonalsDown,DiagonalsUp, D),
	remove_small_diagonals(4, D, Diagonals).