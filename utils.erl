-module(utils).
-export([head/1, tail/1, string_format/2]).

% Failsafe Head
head([]) -> void;
head([H|_]) -> H;
head(Any) -> Any.

% Failsafe Tail
tail([]) -> void;
tail([_|T]) -> T;
tail(Any) -> Any.

string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).