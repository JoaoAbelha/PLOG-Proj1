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

diagonals(Matrix, [L1, L2], N) :-
    findall(B, (between(1,N, I), nth1(I, Matrix, Row), nth1(I, Row, B)), L1),
    findall(B, (between(1,N,I), J is N+1-I, nth1(I, Matrix, Row), nth1(J,Row,B)),L2).