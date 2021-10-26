-module(calc).
-export([ rpn/1, read/1, unit_test/0 ]).


% Reverse Polish notation
rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.

rpn("+", [N1, N2 | S]) -> [N2 + N1|S];
rpn("-", [N1, N2 | S]) -> [N2 - N1|S];
rpn("*", [N1, N2 | S]) -> [N2 * N1|S];
rpn("/", [N1, N2 | S]) -> [N2 / N1|S];
rpn(X, Stack) -> [ read(X) | Stack ].


% Convert a string to a number
read(N) ->
    case string:to_float(N) of
        {error, no_float} -> list_to_integer(N);
        {F, _} -> F
    end.

% Low-tech unit tests
unit_test() ->
    4 = rpn("2 2 +"),
    ok.
