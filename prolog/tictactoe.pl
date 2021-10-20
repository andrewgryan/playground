:- module(tictactoe, [ make_move/3 ]).

% Pattern-match all 9 possible x moves to make new board
make_move([b, B, C, D, E, F, G, H, I], 0, [x, B, C, D, E, F, G, H, I]).
make_move([A, b, C, D, E, F, G, H, I], 1, [A, x, C, D, E, F, G, H, I]).
make_move([A, B, b, D, E, F, G, H, I], 2, [A, B, x, D, E, F, G, H, I]).
make_move([A, B, C, b, E, F, G, H, I], 3, [A, B, C, x, E, F, G, H, I]).
make_move([A, B, C, D, b, F, G, H, I], 4, [A, B, C, D, x, F, G, H, I]).
make_move([A, B, C, D, E, b, G, H, I], 5, [A, B, C, D, E, x, G, H, I]).
make_move([A, B, C, D, E, F, b, H, I], 6, [A, B, C, D, E, F, x, H, I]).
make_move([A, B, C, D, E, F, G, b, I], 7, [A, B, C, D, E, F, G, x, I]).
make_move([A, B, C, D, E, F, G, H, b], 8, [A, B, C, D, E, F, G, H, x]).
make_move(Board, _, Board) :- write("Illegal move"), nl.

