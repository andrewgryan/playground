-module(higher).
-export([ one/0, two/0, add/2, inc/1, dec/1, map/2 ]).

% Higher order function practice

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().


inc(N) -> N + 1.
dec(N) -> N - 1.


map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].
