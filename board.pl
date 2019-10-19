:-include('util.pl').
:-include('list.pl').

getPos(empty,' ').
getPos(green,'G').
getPos(yellow,'Y').

getCel(empty,' ').
getCel(up,'/').
getCel(down,'\\').

firstPlayer(player(1,green)).
secondPlayer(player(2,yellow)).



emptyBoard([[empty,empty, empty, empty, empty],
 	    [empty,empty, empty, empty, empty],
	    [empty,empty, empty, empty, empty],
	    [empty,empty, empty, empty, empty],
	    [empty,empty, empty, empty, empty]]).


e1stateBoard([[empty,green, empty, empty, empty],
 	    [empty,empty, empty, empty, empty],
	    [empty,empty, empty, empty, empty],
	    [empty,empty, empty, green, empty],
	    [empty,empty, empty, empty, yellow]]).

e2stateBoard([[empty,green, yellow, empty, empty],
 	    [empty,empty, empty, empty, empty],
	    [empty,green, empty, green, yellow],
	    [empty,empty, empty, yellow, green],
	    [yellow,empty, empty, empty, empty]]).

finalStateBoard([[green,empty, empty, yellow, empty],
 	    	[empty,green, empty, yellow, empty],
	    	[empty,empty, green, yellow, empty],
	    	[empty,empty, empty, green, empty],
	    	[empty,empty, empty, yellow, empty]]).


cels([	[empty,up, down, empty],[up,up, down, down],[down,down, up, up],[empty, down, up, empty]]).


printGameStatus(NrTurns):-
	    format("~n       #-#-# Straight4 #-#-#~n~n PLAY NUMBER ~d:",[NrTurns]).


printRowNumber(LineNumber) :-
    write(' '), write(LineNumber), write(' ').


printRow_Rest([]).

printRow_Rest([Cel| Rest]):-
    getPos(Cel,Symbol),
    format('____~s',[Symbol]),
    printRow_Rest(Rest).

printRow([Cel|Rest]):-
    getPos(Cel,Symbol),
    format('  ~s',[Symbol]),
    printRow_Rest(Rest).


drawCelsRest([]).

drawCelsRest([Cel| Rest]):-
    getCel(Cel,Symbol),
    format(" ~a  |",[Symbol]),
    drawCelsRest(Rest).


drawCels([Cel|Rest]):-
    write('     |'),
    getCel(Cel,Symbol),
    format(" ~a  |",[Symbol]),
    drawCelsRest(Rest).


printColumnNumbers(N) :-
    write(' '),
    printColumnNumbers_aux(N), nl.

printColumnNumbers_aux(0).

printColumnNumbers_aux(N) :-
    N1 is N - 1,
    printColumnNumbers_aux(N1),
    write('    '),write(N1).



printBoardRest([],[],_).

printBoardRest([HeadOfTheBoard | _],[],LineNumber) :-
    printRowNumber(LineNumber),
    printRow(HeadOfTheBoard),nl.

printBoardRest([HeadOfTheBoard | TailOfTheBoard],[HeadOfTheCells| TailOfTheCells], LineNumber):-
    printRowNumber(LineNumber),
    printRow(HeadOfTheBoard),nl,
    drawCels(HeadOfTheCells),nl,
    NextLineNumber is LineNumber + 1,
    printBoardRest(TailOfTheBoard,TailOfTheCells, NextLineNumber).

printBoard([HeadOfTheBoard | TailOfTheBoard],Cells) :-
    nl,
    length(HeadOfTheBoard, NumberOfColumns),
    printColumnNumbers(NumberOfColumns),
    nl,
    printBoardRest([HeadOfTheBoard | TailOfTheBoard], Cells, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getNrPieces(Board,TypePieces, NumberPieces):-
	getBoardNrElements(Board,NumberPieces, TypePieces).



currentPlayerStatus(player(PlayerNr,TypePieces),Board):-
	getNrPieces(Board,TypePieces, NumberPieces),
        nl,
        format("Player ~d(~s): ~d/4 pieces in the board",[PlayerNr,TypePieces,NumberPieces]).
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call :-
    printGameStatus(4), nl,
    finalStateBoard(M),
    cels(N),
    printBoard(M,N),
    firstPlayer(P1),
    currentPlayerStatus(P1,M).

