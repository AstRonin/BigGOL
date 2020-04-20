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

-export([find_neighbors/1, seed/1, clear/1, check_status/2, ask_status/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).


-record(gol_cell_state, {
%%    state = 0 :: integer(), %% 1 | 0, @todo maybe add -1|0|1 or 0|1|2
    cell = #cell{} :: any(),
    nbrs = [] :: list(),
    nbrs_answers = #{count_answers => 0, count_alive => 0} :: {non_neg_integer(), non_neg_integer()}, %% {Count of answers, Count of alive}
    time_snapshot = 0,
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

check_status(Id, TimeSnapshot) ->
    gen_statem:cast(Id, {check, TimeSnapshot}).

ask_status(From, Id, TimeSnapshot) ->
    gen_statem:cast(Id, {return_status, TimeSnapshot, From}).

%%%===================================================================
%%% Events
%%%===================================================================

%%prepare({call, From}, find_neighbors, Data) ->
%%    {keep_state, Data}.
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

handle_event(cast, {check, TimeSnapshot}, _StateName, State) ->
    {registered_name, MyName} = process_info(self(), registered_name),
    gol_utils:info("Cell ~p start new check with TimeSnapshot: ~p", [MyName, TimeSnapshot]),
    [gol_cell:ask_status(self(), N, TimeSnapshot) || N <- State#gol_cell_state.nbrs],
    State1 = State#gol_cell_state{nbrs_answers = #{count_answers => 0, count_alive => 0}},
    {keep_state, State1};

handle_event(cast, {return_status, TimeSnapshot, From}, StateName, State) -> %% Do return status
    %% Search in the history of versions, If not found retrun current status
    PrevState = proplists:get_value(TimeSnapshot, State#gol_cell_state.v, StateName),
    From ! {status_of_nbr, PrevState, TimeSnapshot},
    keep_state_and_data;

handle_event(info, {status_of_nbr, StatusOfNbr, TimeSnapshot}, StateName, State) -> %% receive nbr status
    #gol_cell_state{
        nbrs = Nbr,
        nbrs_answers = #{count_answers := CountAnswers, count_alive => CountAlive},
        v = V
    } = State,

    CountAnswers1 = CountAnswers + 1,
    Sum = status_to_int(StatusOfNbr) + CountAlive,

    if
        CountAnswers1 =:= length(Nbr) ->

            NewStateName = fate(StateName, Sum),

            State1 = State#gol_cell_state{
                nbrs_answers = #{count_answers := 0, count_alive => 0},
                v = [{} | V]
            },

            {registered_name, MyName} = process_info(self(), registered_name),
            gol_demiurge ! {done, MyName},
            {next_status, NewStateName, State1};
        true ->
            State1 = State#gol_cell_state{
                nbrs_answers = #{count_answers := CountAnswers1, count_alive => Sum}
            },
            {keep_state, State1}
    end;
handle_event(info, {status_of_nbr, _StatusOfNbr, TimeSnapshot}, _StateName, #gol_cell_state{time_snapshot = TimeSnapshot}) ->
    gol_utils:error("TimeSnapshot ~p is not correct, please be slow!", [TimeSnapshot]),
    keep_state_and_data;

handle_event(_EventType, EventContent, _StateName, _State) ->
    gol_utils:info("Unsupported EventContent: ~p", [EventContent]),
    keep_state_and_data.

fate(?STATUS_DEAD, Num) ->
    {Born, _Survive} = gol_utils:config(life_rule),
    fate(Num, Born);
fate(?STATUS_ALIVE, Num) ->
    {_Born, Survive} = gol_utils:config(life_rule),
    fate(Num, Survive);
fate(Num, List) ->
    case lists:member(Num, List) of
        true -> ?STATUS_ALIVE;
        false -> ?STATUS_DEAD
    end.


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

status_to_int(?STATUS_ALIVE) ->
    1;
status_to_int(?STATUS_DEAD) ->
    0;
status_to_int(_) ->
    0.