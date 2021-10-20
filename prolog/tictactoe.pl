:- module(tictactoe, [ turn/3 ]).


% Win criteria
win(Board, Player) :- win_diagonal(Board, Player).
win(Board, Player) :- win_column(Board, Player).
win(Board, Player) :- win_row(Board, Player).
win_diagonal(Board, Player) :-
    Board = [
        _, _, Player,
        _, Player, _,
        Player, _, _
    ].
win_diagonal(Board, Player) :-
    Board = [
        Player, _, _,
        _, Player, _,
        _, _, Player
    ].
win_row(Board, Player) :-
    Board = [
        Player, Player, Player,
        _, _, _,
        _, _, _
    ].
win_row(Board, Player) :-
    Board = [
        _, _, _,
        Player, Player, Player,
        _, _, _
    ].
win_row(Board, Player) :-
    Board = [
        _, _, _,
        _, _, _,
        Player, Player, Player
    ].
win_column(Board, Player) :-
    Board = [
        Player, _, _,
        Player, _, _,
        Player, _, _
    ].
win_column(Board, Player) :-
    Board = [
        _, Player, _,
        _, Player, _,
        _, Player, _
    ].
win_column(Board, Player) :-
    Board = [
        _, _, Player,
        _, _, Player,
        _, _, Player
    ].


% Single player turn system
turn(Board, Move, NewBoard) :-
    x_move(Board, Move, B),
    o_move(B, NewBoard).
    

% All possible moves/boards
move([b, B, C, D, E, F, G, H, I], Player, [Player, B, C, D, E, F, G, H, I]).
move([A, b, C, D, E, F, G, H, I], Player, [A, Player, C, D, E, F, G, H, I]).
move([A, B, b, D, E, F, G, H, I], Player, [A, B, Player, D, E, F, G, H, I]).
move([A, B, C, b, E, F, G, H, I], Player, [A, B, C, Player, E, F, G, H, I]).
move([A, B, C, D, b, F, G, H, I], Player, [A, B, C, D, Player, F, G, H, I]).
move([A, B, C, D, E, b, G, H, I], Player, [A, B, C, D, E, Player, G, H, I]).
move([A, B, C, D, E, F, b, H, I], Player, [A, B, C, D, E, F, Player, H, I]).
move([A, B, C, D, E, F, G, b, I], Player, [A, B, C, D, E, F, G, Player, I]).
move([A, B, C, D, E, F, G, H, b], Player, [A, B, C, D, E, F, G, H, Player]).
move(Board, _, Board).

% o makes next possible move
o_move(Board, NewBoard) :-
    move(Board, o, NewBoard),
    win(NewBoard, o),
    !.
o_move(Board, NewBoard) :-
    move(Board, o, NewBoard),
    not(x_win_in_one(NewBoard)).
o_move(Board, NewBoard) :-
    move(Board, o, NewBoard).

x_win_in_one(Board) :-
    move(Board, x, NewBoard),
    win(NewBoard, x).

% Pattern-match all 9 possible x moves to make new board
x_move([b, B, C, D, E, F, G, H, I], 0, [x, B, C, D, E, F, G, H, I]).
x_move([A, b, C, D, E, F, G, H, I], 1, [A, x, C, D, E, F, G, H, I]).
x_move([A, B, b, D, E, F, G, H, I], 2, [A, B, x, D, E, F, G, H, I]).
x_move([A, B, C, b, E, F, G, H, I], 3, [A, B, C, x, E, F, G, H, I]).
x_move([A, B, C, D, b, F, G, H, I], 4, [A, B, C, D, x, F, G, H, I]).
x_move([A, B, C, D, E, b, G, H, I], 5, [A, B, C, D, E, x, G, H, I]).
x_move([A, B, C, D, E, F, b, H, I], 6, [A, B, C, D, E, F, x, H, I]).
x_move([A, B, C, D, E, F, G, b, I], 7, [A, B, C, D, E, F, G, x, I]).
x_move([A, B, C, D, E, F, G, H, b], 8, [A, B, C, D, E, F, G, H, x]).
x_move(Board, _, Board).

