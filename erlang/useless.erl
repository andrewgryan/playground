-module(useless).
-export([hello_world/0, factorial/1]).


% Obligatory Hello, World! function
hello_world() ->
    io:format("Hello, World!~n").


% Traditional factorial function
factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).
