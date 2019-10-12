

getCel(empty,' ').
getCel(black,'O').
getCel(white, 'B').


getPos(empty,' ').
getPos(up,'   ->').
getPos(down,'  <-').


type1([One, Two, Three, Four]) :-
    getPos(One,O),getPos(Two,Tw),
    getPos(Three,Th), getPos(Four,F),
   format('| ~s~t~11|| ~s~t~22|| ~s~t~33|| ~s~t~44||~n', 
                            [ O  , Tw  , Th, F]).
     

printRow_Rest([]).
printRow_Rest([Cel| Rest]):-
    getCel(Cel,Symbol),
    format(' ________ ~s',[Symbol]),
    printRow_Rest(Rest).

printRow([Cel|Rest]):-
    getCel(Cel,Symbol),
    format('~s',[Symbol]),
    printRow_Rest(Rest).


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
	



printBoard([Row|_], []):-
     printRow(Row),nl.

    
printBoard([Row | Rest], [Cel| R]):-
    printRow(Row),nl,
    type1(Cel),
    printBoard(Rest,R).


showBoard(Pieces,Cels,PlayNr):-
   	PlayNr < 4,
    format("Straight4 # Play Number ~d:",[PlayNr]),nl,
    printBoard(Pieces,Cels),
    getNrPieces(Pieces,White,Black),
    format("Player A: ~d pieces~nPlayer B: ~d pieces~n",[White,Black]).

showBoard(Pieces,Cels,PlayNr):-
    format("Straight4 # Play Number ~d:",[PlayNr]),nl,
    printBoard(Pieces,Cels),
    getNrPieces(Pieces,White,Black),
    format("Player A: ~d pieces~nPlayer B: ~d pieces~n",[4,4]).





    
    
    
    
    
    