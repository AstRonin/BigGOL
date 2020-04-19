%%%-------------------------------------------------------------------
%%% @author Roman Shuplov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2020 0:23
%%%-------------------------------------------------------------------
-module(gol_utils).
-author("Roman Shuplov").

-include("gol.hrl").

%% API
-compile(export_all).

config(Key) -> config(sgi, Key, "").
config(Key, Default) -> config(sgi, Key, Default).
config(App, Key, Default) -> application:get_env(App, Key, Default).

-spec key(#cell{}) -> atom().
key(#cell{row = Row, col = Col, layer = Layer}) ->
    key(Row, Col, Layer).
-spec key(integer(), integer()) -> atom().
key(Row, Col) ->
    key(Row, Col, 0).
-spec key(integer(), integer(), integer()) -> atom().
key(Row, Col, Layer) ->
    list_to_atom(lists:concat([Row, ":", Col, ":", Layer])).

-spec neighbors(#cell{}) -> list().
neighbors(#cell{row = Row, col = Col, layer = Layer}) ->
    Try = [-1, 0, 1],
    [key(#cell{row = Row + NRow, col = Col + NCol, layer = Layer}) ||
        NRow <- Try,
        NCol <- Try,
        (Row + NRow) > -1,
        (Col + NCol) > -1
    ].

info(String, Args) ->
    ok = logger:info(String ++ "~n", Args).

info(String) ->
    ok = logger:info(String ++ "~n").

error(String, Args) ->
    ok = logger:error(String ++ "~n", Args).

error(String) ->
    ok = logger:error(String ++ "~n").

%% get current time in milliseconds
-spec get_timestamp() -> integer().
get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).