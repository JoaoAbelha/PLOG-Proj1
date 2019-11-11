use_module(library(system)).

%%prints N numbers in one line [Start, Total[
printSeqNumbers(Start,Total):-
    Start =< Total,
    printSeqNumbers_aux(Start,Total).

%% check if are equal
equal(A,A).


setRandomSeed:-
	now(Time), S is Time mod 30269,
	getrand(random(X, Y, Z, _)),
	setrand(random(S, X, Y, Z)), !.
:-setRandomSeed. 



clearConsole:-
	:-write('\33\[2J').


	
