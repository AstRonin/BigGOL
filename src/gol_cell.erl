%%%-------------------------------------------------------------------
%%% @author Roman Shuplov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2020 18:36
%%%-------------------------------------------------------------------
-module(gol_cell).
-author("Roman Shuplov").

-behaviour(gen_statem).

-define(STATUS_PREPARE, prepare).
-define(STATUS_ALIVE, alive).
-define(STATUS_DEAD, dead).

%%-define(HANDLE_COMMON,
%%    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).

-include("gol.hrl").

%% API
-export([start_link/0]).

-export([find_neighbors/1, seed/1, clear/1, check/2]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(gol_cell_state, {
%%    state = 0 :: integer(), %% 1 | 0, @todo maybe add -1|0|1 or 0|1|2
    cell = #cell{} :: any(),
    nbrs = [] :: list(),
    v = [] :: [{non_neg_integer(), atom()}] %% prepare, alive, dead
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec find_neighbors(atom()) -> ok.
find_neighbors(Id) ->
    gen_statem:cast(Id, find_neighbors).

seed(Id) ->
    gen_statem:cast(Id, seed).

clear(Id) ->
    gen_statem:cast(Id, clear).

check(Id, TimeSnapshot) ->
    gen_statem:cast(Id, {check, TimeSnapshot}).

%%%===================================================================
%%% Events
%%%===================================================================

%%prepare({call, From}, find_neighbors, Data) ->
%%
%%    {keep_state, Data}.
%%
%%
%%
%%handle_common({call, From}, find_neighbors, Data) ->
%%    {keep_state, Data,
%%        [{reply, From, ""}]}.
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([Cell]) ->
    Data = #gol_cell_state{cell = Cell, v = [{0, ?STATUS_PREPARE}]},
    {ok, ?STATUS_PREPARE, Data}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    state_functions. %% handle_event_function

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #gol_cell_state{}) ->
    NextStateName = next_state,
    {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(cast, find_neighbors, _StateName, State) ->
    Nbrs = gol_utils:neighbors(State#gol_cell_state.cell),
    {keep_state, State#gol_cell_state{nbrs = Nbrs}};
handle_event(cast, seed, _StateName, State) ->
    {next_state, ?STATUS_ALIVE, State};
handle_event(cast, clear, _StateName, State) ->
    {next_state, ?STATUS_DEAD, State};
handle_event(cast, check, _StateName, State) ->


    {next_state, ?STATUS_DEAD, State};
handle_event(_EventType, _EventContent, _StateName, State = #gol_cell_state{}) ->
    NextStateName = the_next_state_name,
    {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #gol_cell_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #gol_cell_state{}, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================