%%prints N numbers in one line [Start, Total[

printSeqNumbers(Start,Total):-
    Start =< Total,
    printSeqNumbers_aux(Start,Total).


%%printNumbers_aux(N,N).
%%printNumbers_aux(Number,Total):-
%%	NextNr is Number + 1,
%%	write('     '),write(Number),
%%	printNumbers_aux(NextNr,Total).

%% check if are equal
equal(A,A).
	
