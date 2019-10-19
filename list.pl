
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


	
	
	


