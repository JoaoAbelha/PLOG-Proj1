:-use_module(library(lists)).

diagonal_down([Row1, Row2, Row3, Row4|_], [Color1,Color2,Color3,Color4]):-
    nth0(Index1,Row1,Color1),
    Index2 is Index1 + 1,
    nth0(Index2,Row2,Color2),
    Index3 is Index2 + 1,
    nth0(Index3,Row3,Color3),
    Index4 is Index3 + 1,
    nth0(Index4,Row4, Color4).

diagonal_up([Row1, Row2, Row3, Row4|_], [Color4,Color3,Color2,Color1]):-
    nth0(Index1,Row1,Color1),
    Index2 is Index1 - 1,
    nth0(Index2,Row2,Color2),
    Index3 is Index2 - 1,
    nth0(Index3,Row3,Color3),
    Index4 is Index3 - 1,
    nth0(Index4,Row4, Color4).


board([[empty,  empty,  empty, empty,  empty],
       [green,  green,  green, empty,  green],
       [empty,  yellow, empty, empty,  yellow],
       [yellow, empty,  empty, yellow, empty],
       [empty,  empty,  empty, empty,  empty]
]).


diagonals([RowStart|Rest], Resultant, Tamanho, RowAtual, InLine):-
    RowFinal is RowAtual + InLine,
    RowFinal =< Tamanho,
    New is RowAtual + 1,
    findall(X,diagonal_up([RowStart|Rest],X),Result1),
    findall(Y,diagonal_down([RowStart|Rest],Y),Result2),
    diagonals(Rest,RestResult,Tamanho, New, InLine),
   	append(Result1,Result2,Result3),
    append(Result3,RestResult,Resultant).


diagonals([_|Rest], RestResult,Tamanho, RowAtual, InLine):-
    RowFinal is RowAtual + InLine,
    RowFinal =< Tamanho,
    New is RowAtual + 1,
    diagonals(Rest,RestResult,Tamanho, New, InLine).

diagonals(_,[],_,_,_).

call4(Res):-
    board(B),
    getAllDiagonals(B,R,5,0,4), !,
    sort(R,Res).

call5(X):-
    board(B),
    getDiagonal_Down(B,X).

%%call4(R):-
	%%board([H|N]),
    %%findall(X,getDiagonal([H|N],X),R1),
    %%findall(Y, getDiagonal(N, Y), R2),
    %%append(R1,R2,R).
