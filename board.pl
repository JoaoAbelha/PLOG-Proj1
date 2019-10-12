

getCel(empty,' ').
getCel(black,'O').
getCel(white, 'B').


getPos(empty,' ').
getPos(up,'   ->').
getPos(down,'  <-').


type1([One, Two, Three, Four]) :-
    getPos(One,O),getPos(Two,Tw),
    getPos(Three,Th), getPos(Four,F),
   format('   | ~s~t~13||  ~s~t~24|| ~s~t~35|| ~s~t~46||~n', 
                            [ O  , Tw  , Th, F]).

colsId:- format('   a~t~12| b~t~23| c~t~34| d~t~45| e~n').
    
     


getPiecesOnLine([],0,0).
getPiecesOnLine([Row|Rest],White,Black):-
	Row==empty -> getPiecesOnLine(Rest, White, Black);
    Row == white -> getPiecesOnLine(Rest, White1, Black),White is White1+1;
    Row == black -> getPiecesOnLine(Rest, White, Black1),Black is Black1+1.


getNrPieces([],0,0).
getNrPieces([SubList | Rest],White,Black):-
    getPiecesOnLine(SubList, White1, Black1),
    getNrPieces(Rest,White2, Black2),
    White is White1 + White2,
    Black is Black1 + Black2.
	

printRow_Rest([]).
printRow_Rest([Cel| Rest]):-
    getCel(Cel,Symbol),
    format('________ ~s ',[Symbol]),
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
    type1(Cel),
    printBoard(Rest,R,Nr1).


showBoard(Pieces,Cels,PlayNr):-
   	PlayNr < 4,
    format("Straight4 # Play Number ~d:",[PlayNr]),nl,nl,
    colsId,nl,
    printBoard(Pieces,Cels,1),
    getNrPieces(Pieces,White,Black),
    format("Player A: ~d pieces~nPlayer B: ~d pieces~n",[White,Black]).

showBoard(Pieces,Cels,PlayNr):-
    format("Straight4 # Play Number ~d:",[PlayNr]),nl,nl,
    colsId,nl,
    printBoard(Pieces,Cels,1),
    format("Player A: ~d pieces~nPlayer B: ~d pieces~n",[4,4]).