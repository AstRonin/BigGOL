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

config(Key) -> config(gol, Key, "").
config(Key, Default) -> config(gol, Key, Default).
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

%% '0:0:0' , Found Nbrs: ['0:1:0','1:0:0','1:1:0']
-spec neighbors(#cell{}, #field{}) -> list().
neighbors(#cell{row = Row, col = Col, layer = Layer}, #field{rows = MaxRow, cols = MaxCol}) ->
    Try = [-1, 0, 1],
    [key(#cell{row = Row + NRow, col = Col + NCol, layer = Layer}) ||
        NRow <- Try,
        NCol <- Try,
        (Row + NRow) > -1, % do not -1 upper then first row
        (Col + NCol) > -1, % do not -1 left the first col
        (Row + NRow) < MaxRow, % do not more than row
        (Col + NCol) < MaxCol, % do not more than col
        not (NRow =:= 0 andalso NCol =:= 0) % do not self
    ].

log_info(String) ->
    log_info(String, []).
log_info(String, Args) ->
    ok = logger:info("~p , " ++ String ++ "~n", [proc_name() | Args]).

log_warning(String) ->
    log_warning(String, []).
log_warning(String, Args) ->
    ok = logger:warning("~p , " ++ String ++ "~n", [proc_name() | Args]).

log_error(String) ->
    log_error(String, []).
log_error(String, Args) ->
    ok = logger:error("~p , " ++ String ++ "~n", [proc_name() | Args]).


proc_name() ->
    case process_info(self(), registered_name) of
        {registered_name, N} -> N;
        _ -> self()
    end.

%% get current time in milliseconds
-spec get_timestamp() -> integer().
get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).
