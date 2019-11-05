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
	


	
	


