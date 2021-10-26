-module(tree).
-export([ empty/0, insert/3, lookup/2 ]).


empty() -> {node, 'nil'}.


% Algorithm to insert key value node
insert(Key, Value, {node, 'nil'}) ->
    {node, {Key, Value, {node, 'nil'}, {node, 'nil'}}};

insert(NewKey, NewValue, {node, {Key, Value, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Value, insert(NewKey, NewValue, Smaller), Larger}};

insert(NewKey, NewValue, {node, {Key, Value, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Value, Smaller, insert(NewKey, NewValue, Larger)}};

insert(Key, Value, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Value, Smaller, Larger}}.


% Algorithm to lookup value in tree
lookup(_, {node, 'nil'}) -> undefined;

lookup(NewKey, {node, {Key, _, Smaller, _}}) when NewKey < Key ->
    lookup(NewKey, Smaller);

lookup(NewKey, {node, {Key, _, _, Larger}}) when NewKey > Key ->
    lookup(NewKey, Larger);

lookup(Key, {node, {Key, Value, _, _}}) ->
    {ok, Value}.
