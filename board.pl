:- use_module(library(between)).
:-ensure_loaded('list.pl').

getPos(empty,' ').
getPos(green,'G').
getPos(yellow,'Y').

getCel(empty,' ').
getCel(up,'/').
getCel(down,'\\').

emptyBoard([[empty,empty, empty, empty, empty],
        [empty,empty, empty, empty, empty],
        [empty,empty, empty, empty, empty],
        [empty,empty, empty, empty, empty],
        [empty,empty, empty, empty, empty]
]).


e1stateBoard([[empty,empty, empty, empty, empty],
              [green,empty, empty, yellow, empty],
              [empty,empty, empty, empty, empty],
              [empty,empty, empty, empty, empty],
              [empty,empty, empty, empty, empty]
]).


e2stateBoard([[empty,empty, empty, empty, empty],
              [green,green, green, empty, green],
              [empty,yellow, empty, empty, yellow],
              [yellow,empty, empty, yellow, empty],
              [empty,empty, empty, empty, empty]
]).

finalStateBoard([[empty,empty, empty, empty, empty],
            [green,green, green, yellow, empty],
            [green,empty, empty, yellow, empty],
            [empty,empty, empty, yellow, empty],
            [empty,empty, empty, yellow, empty]
]).

cels([[empty,up, down, empty],
    [up,up, down, down],
    [down,down, up, up],
    [empty, down, up, empty]
]).

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
    printBoardRest([HeadOfTheBoard | TailOfTheBoard], Cells, 0), !.

getNrPieces(Board,TypePieces, NumberPieces):-
	getBoardNrElements(Board,NumberPieces, TypePieces).

currentPlayerStatus(player(Piece, _), Board):-
	getNrPieces(Board, Piece, NumberPieces),
    nl,
    format("Player ~s: ~d/4 pieces in the board",[Piece,NumberPieces]), !.

insideBoard([HeadOfTheBoard | TailOfTheBoard], X, Y) :-
    length(HeadOfTheBoard, BoardLengthY),
    BoardLenY is BoardLengthY - 1,
    between(0, BoardLenY, Y),
    length(TailOfTheBoard, BoardLenX),
    between(0, BoardLenX, X).