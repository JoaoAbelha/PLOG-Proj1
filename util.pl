%%prints N numbers in one line [Start, Total[

printSeqNumbers(Start,Total):-
    Start =< Total,
    printSeqNumbers_aux(Start,Total).

%% check if are equal
equal(A,A).
	
