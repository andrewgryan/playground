-module(useless).
-export([hello_world/0, factorial/1, len/1, repeat/2]).


% Obligatory Hello, World! function
hello_world() ->
    io:format("Hello, World!~n").


% Traditional factorial function
factorial(N) -> factorial(N, 1).
factorial(0, F) -> F;
factorial(N, F) when N > 0 -> factorial(N - 1, N * F).


% Example list length
len(T) -> len(T, 0).
len([], N) -> N;
len([_|T], N) -> len(T, N+1).


% Repeat value N times
repeat(V, N) -> repeat(V, N, []).
repeat(_, 0, Acc) -> Acc;
repeat(V, N, Acc) when N > 0 -> repeat(V, N - 1, [V | Acc]).
