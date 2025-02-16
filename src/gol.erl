%%%-------------------------------------------------------------------
%%% @author Roman Shuplov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2020 19:51
%%%-------------------------------------------------------------------
-module(gol).
-author("Roman Shuplov").

-behaviour(application).

-include("gol.hrl").

-export([run/0, run/1, wait/0, seed/1, clear/0, get_alive/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%% Api

run() ->
    gol_demiurge:run().
run(Num) ->
    gol_demiurge:run(Num).

wait() ->
    gol_demiurge:wait().

seed(Seed) ->
    gol_demiurge:seed_cells(Seed).

clear() ->
    gol_demiurge:clear_cells().

get_alive() ->
    gol_demiurge:get_alive().



%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    case gol_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
