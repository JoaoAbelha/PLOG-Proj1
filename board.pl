getCel(empty,' ').
getCel(black,'O').
getCel(white, '#').


getPos(empty,' ').
getPos(up,'/').
getPos(down,'\\').


%%initial_state
emptyBoard([ [empty,empty, empty, empty, empty], 
	     [empty,empty, empty, empty, empty],
	     [empty,empty, empty, empty, empty],
	     [empty,empty, empty, empty, empty],
	     [empty,empty, empty, empty, empty] ]).
  
cels([	[empty,up, down, empty],
	[up,up, down, down],
	[down,down, up, up],
	[empty, down, up, empty]]).


%% desenha as celulas do tabuleiro que podem ser de 3 tipos
drawCels([TypeOne, TypeTwo, TypeThree, TypeFour]) :-
    getPos(TypeOne,O),getPos(TypeTwo,Tw),
    getPos(TypeThree,Th), getPos(TypeFour,F),
    format("~3||~t~a~t~6+|~t~a~t~7+|~t~a~t~7+|~t~a~t~6+|~n", [O, Tw, Th, F]).

%% desenha os identificadores das colunas encima do tabuleiro
colsId:- format("~3|~d~t~t~6+~d~t~t~7+~d~t~t~7+~d~t~t~6+~d~n", [1,2,3,4,5]).
    

%% nr de peças de cada cor presentes numa linha     
getPiecesOnLine([],0,0).
getPiecesOnLine([Row|Rest],White,Black):-
    Row==empty -> getPiecesOnLine(Rest, White, Black);
    Row == white -> getPiecesOnLine(Rest, White1, Black),White is White1+1;
    Row == black -> getPiecesOnLine(Rest, White, Black1),Black is Black1+1.


%% peças de cada cor no tabuleiro
getNrPieces([],0,0).
getNrPieces([SubList | Rest],White,Black):-
    getPiecesOnLine(SubList, White1, Black1),
    getNrPieces(Rest,White2, Black2),
    White is White1 + White2,
    Black is Black1 + Black2.
	

printRow_Rest([]).
printRow_Rest([Cel| Rest]):-
    getCel(Cel,Symbol),
    format('____ ~s ',[Symbol]),
    printRow_Rest(Rest).

printRow([Cel|Rest],Nr):-
    getCel(Cel,Symbol),
    format('~d  ~s',[Nr,Symbol]),
    printRow_Rest(Rest).


printBoard([Row|_], [],Nr):-
     printRow(Row,Nr),nl.

    
printBoard([Row | Rest], [Cel| R],Nr):-
    printRow(Row,Nr),nl,
    Nr1 is Nr + 1,
    drawCels(Cel),
    printBoard(Rest,R,Nr1).

showBoard(Pieces,Cels,PlayNr):-
   	PlayNr < 4,
    format("Straight4 # Play Number ~d:",[PlayNr]),nl,nl,
    colsId,nl,
    printBoard(Pieces,Cels,1),
    getNrPieces(Pieces,White,Black),nl,
    format("Player A: ~d pieces~nPlayer B: ~d pieces~n~n",[White,Black]).

showBoard(Pieces,Cels,PlayNr):-
    format("Straight4 # Play Number ~d:",[PlayNr]),nl,nl,
    colsId,nl,
    printBoard(Pieces,Cels,1),nl,
    format("Player A: ~d pieces~nPlayer B: ~d pieces~n~n",[4,4]),nl.



call :-	emptyBoard(M),cels(N),
	showBoard(M,N,4).
