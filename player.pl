:- use_module(library(system)).
:- ensure_loaded('player_chooser.pl').
:- ensure_loaded('input.pl').

initial_player(player(green, Type)) :-
    choose_player(Type).

second_player(player(yellow, Type)) :-
    choose_player(Type).

is_human(1).
is_random_ai(2).
is_smart_ai(3).

get_random_element(List, Element) :-
    length(List, _N),
    random(0, _N, RandN),
    nth0(RandN, List, Element), !.

get_move(Type, _, Move, _, _, NTurns) :-
    is_human(Type), !,
    (
        NTurns =< 8,
        read_coords(Col, Row),
        Move = move(Col, Row);
        NTurns > 8,
        read_coords(SrcCol, SrcRow),
        read_coords(DestCol, DestRow),
        Move = move(SrcCol, SrcRow, DestCol, DestRow)
    ).

get_move(Type, game_state(Board, Cels, _, _), Move, Piece, _, NTurns) :-
    is_random_ai(Type), !,
    (
        NTurns =< 8,
        findall(M, valid_move(Board, M), Moves), write(Moves), sleep(1),
        get_random_element(Moves, Move);
        NTurns > 8,
        findall(M, valid_move(Board, Cels, M, Piece), Moves), sleep(1),
        get_random_element(Moves, Move)
    ).