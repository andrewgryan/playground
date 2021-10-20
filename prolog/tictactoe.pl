:- module(tictactoe, [ turn/3 ]).


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

% o makes next possible move
o_move(Board, NewBoard) :-
    move(Board, o, NewBoard).

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
x_move(Board, _, Board) :- write("Illegal move"), nl.

