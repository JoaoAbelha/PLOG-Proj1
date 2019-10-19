getPos(empty,' ').
getPos(green,'G').
getPos(white,'W').

getCel(empty,' ').
getCel(up,'/').
getCel(down,'\\').

firstPlayer(player(1,green)).
secondPlayer(player(2,white)).

%%initial_state
emptyBoard([
        [empty,empty, empty, empty, empty], 
	    [empty,empty, empty, empty, empty],
	    [empty,empty, empty, empty, empty],
	    [empty,empty, empty, empty, empty],
	    [empty,empty, empty, empty, empty]
    ]).
  
cels([	
    [empty,up, down, empty],
	[up,up, down, down],
	[down,down, up, up],
	[empty, down, up, empty]]).


drawCelsRest([]).
drawCelsRest([Cel| Rest]):-
    getCel(Cel,Symbol),
    format("  ~a  |",[Symbol]),
    drawCelsRest(Rest).

drawCels([Cel|Rest]):-
    write('     |'),
    getCel(Cel,Symbol),
    format("  ~a  |",[Symbol]),
    drawCelsRest(Rest).

%% nr de peças de cada cor presentes numa linha     
getNrPiecesLine([],green,0).
getNrPiecesLine([Row|Rest],Piece,PieceNr):-
    Row \== Piece -> getNrPiecesLine(Rest, Piece, PieceNr);
    Row == Piece -> getNrPiecesLine(Rest, Piece, PieceNr1), PieceNr is PieceNr1+1;

%% peças de cada cor no tabuleiro
getNrPieces([],green,0).
getNrPieces([SubList | Rest],Piece,PieceNr):-
    getNrPieces(Rest,Piece,0),
    getNrPiecesLine(SubList,Piece, Piece1),
    PieceNr is Piece1 + Piece2.

printRow_Rest([]).
printRow_Rest([Cel| Rest]):-
    getPos(Cel,Symbol),
    format('____ ~s',[Symbol]),
    printRow_Rest(Rest).

printRow([Cel|Rest]):-
    getPos(Cel,Symbol),
    format('  ~s',[Symbol]),
    printRow_Rest(Rest).

/*printBoard([Row|_], [],Nr):-
    printRow(Row,Nr),nl.
    
printBoard([Row | Rest], [Cel| R],Nr):-
    printRow(Row,Nr),nl,
    Nr1 is Nr + 1,
    drawCels(Cel),
    printBoard(Rest,R,Nr1).*/

printColumnNumbers(N) :-
    printColumnNumbers2(N), nl.

printColumnNumbers2(0).

printColumnNumbers2(N) :-
    N1 is N - 1,
    printColumnNumbers2(N1),
    write('     '),write(N1).

printRowNumber(LineNumber) :-
    write(' '), write(LineNumber), write(' ').

sizeList([], 0).

sizeList([_ | Tail], Size) :-
    sizeList(Tail, SizeOfTail),
    Size is SizeOfTail + 1.

printBoard([HeadOfTheBoard | TailOfTheBoard],Cells) :-
    nl,
    sizeList(HeadOfTheBoard, NumberOfColumns),
    printColumnNumbers(NumberOfColumns),
    nl,
    printBoard2([HeadOfTheBoard | TailOfTheBoard], Cells, 0).


printBoard2([],[],_).

printBoard2([HeadOfTheBoard | TailOfTheBoard],[],LineNumber) :-
    printRowNumber(LineNumber),
    printRow(HeadOfTheBoard),nl.

printBoard2([HeadOfTheBoard | TailOfTheBoard],[HeadOfTheCells| TailOfTheCells], LineNumber):-
    printRowNumber(LineNumber),
    printRow(HeadOfTheBoard),nl,
    drawCels(HeadOfTheCells),nl,
    NextLineNumber is LineNumber + 1,
    printBoard2(TailOfTheBoard,TailOfTheCells, NextLineNumber).

printNTurns(NTurns) :-
    format("Straight4 # Play Number ~d:",[NTurns]).

printPlayer(player(PlayerNr,Piece), Board) :-
    getNrPieces(Board,Piece,PieceNr),
    nl,
    format("Player ~d: ~d pieces",[PlayerNr,4]).


showBoard(Board,Cels,Player,NTurns):-
   	NTurns < 4,nl,
    printNTurns(NTurns),nl,
    printBoard(Board,Cels),nl,
    printPlayer(Player,Board).

/*showBoard(Board,Cels,PlayNr):-
    format("Straight4 # Play Number ~d:",[PlayNr]),nl,nl,
    colsId,nl,
    printBoard(Pieces,Cels,1),nl,
    format("Player A: ~d pieces~nPlayer B: ~d pieces~n~n",[4,4]),nl.*/

call :-
    firstPlayer(P1),
    emptyBoard(M),
    cels(N),
    showBoard(M,N,P1,1).
    
