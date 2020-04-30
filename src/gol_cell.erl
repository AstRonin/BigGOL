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

-define(PD_CELL, cell). %% #cell{}
-define(PD_FIELD, field). %% #field{}
-define(PD_LIFE_RULE, life_rule). %% @Todo maybe move to the State

%%-define(HANDLE_COMMON,
%%    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).

-include("gol.hrl").

%% API
-export([start_link/3, start/3]).

-export([find_neighbors/1, seed/1, clear/1, is_alive/1, current_status/1, check_status/2, ask_status/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).


-record(gol_cell_state, {
%%    state = 0 :: integer(), %% 1 | 0, @todo maybe add -1|0|1 or 0|1|2
%%    cell = #cell{} :: any(),
    nbrs = [] :: [atom()],
    nbrs_count = 0 :: non_neg_integer(),
    nbrs_answers = #{answer_count => 0, alive_count => 0} :: #{answer_count => non_neg_integer(), alive_count => non_neg_integer()}, %% {Count of answers, Count of alive}
    time_snapshot = undefined :: {atom(), atom()} | undefined %% prepare, alive, dead
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Cell, Field) ->
    gen_statem:start_link({local, Name}, ?MODULE, [Cell, Field], []).

start(Name, Cell, Field) ->
    gen_statem:start({local, Name}, ?MODULE, [Cell, Field], []).

-spec find_neighbors(atom()) -> ok.
find_neighbors(Id) ->
    gen_statem:cast(Id, find_neighbors).

seed(Id) ->
    gen_statem:cast(Id, seed).

clear(Id) ->
    gen_statem:cast(Id, clear).

current_status(Id) ->
    gen_statem:call(Id, current_status).

-spec is_alive(atom()|pid()) -> boolean().
is_alive(Id) ->
    gen_statem:call(Id, is_alive).

check_status(Id, TimeSnapshot) ->
    gen_statem:cast(Id, {check, TimeSnapshot}).

ask_status(From, Id, TimeSnapshot) ->
    gen_statem:cast(Id, {return_nbr_status, TimeSnapshot, From}).

%%%===================================================================
%%% Events
%%%===================================================================

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([Cell, Field]) ->

    process_flag(message_queue_data, off_heap),

%%    gol_utils:info("INIT ~p, ~p", [Cell, Field]),
    put(?PD_CELL, Cell),
    put(?PD_FIELD, Field),
    put(?PD_LIFE_RULE, gol_utils:config(life_rule)),
    {ok, ?STATUS_DEAD, #gol_cell_state{}}.
%%    {ok, ?STATUS_PREPARE, #gol_cell_state{}}.

handle_event({call, From}, current_status, StateName, _State) ->
    {keep_state_and_data, [{reply, From, StateName}]};

handle_event({call, From}, is_alive, StateName, _State) ->
    {keep_state_and_data, [{reply, From, StateName =:= ?STATUS_ALIVE}]};


handle_event(cast, find_neighbors, _StateName, State) ->
    Nbrs = gol_utils:neighbors(get(?PD_CELL), get(?PD_FIELD)),
%%    gol_utils:info("Found Nbrs: ~p", [Nbrs]),
    {keep_state, State#gol_cell_state{nbrs = Nbrs, nbrs_count = length(Nbrs)}};

handle_event(cast, {life_rule, R}, _StateName, _State) ->
    put(?PD_LIFE_RULE, R),
    keep_state_and_data;

handle_event(cast, seed, _StateName, State) ->
    gol_utils:log_info("Seed as Alive"),
    {next_state, ?STATUS_ALIVE, State#gol_cell_state{time_snapshot = undefined}};

handle_event(cast, clear, _StateName, State) ->
    gol_utils:log_info("Cleared"),
    {next_state, ?STATUS_DEAD, State#gol_cell_state{time_snapshot = undefined}};

handle_event(cast, {check, TimeSnapshot}, StateName, State) ->
%%    gol_utils:info("start new check with TimeSnapshot: ~p", [TimeSnapshot]),

    [gol_cell:ask_status(self(), N, TimeSnapshot) || N <- State#gol_cell_state.nbrs],

    State1 = State#gol_cell_state{
        nbrs_answers = #{answer_count => 0, alive_count => 0},
        time_snapshot = {TimeSnapshot, StateName}
    },
    {keep_state, State1};

handle_event(cast, {return_nbr_status, TimeSnapshot, From}, StateName, State) -> %% Do return status

    CurrentStatus =
        case State#gol_cell_state.time_snapshot of
            undefined -> StateName;
            {TimeSnapshot, SavedStateName} -> SavedStateName;
            _ -> StateName %% for safe
        end,

    From ! {status_of_nbr, CurrentStatus},
    keep_state_and_data;

handle_event(info, {status_of_nbr, StatusNbr}, StateName, State) -> %% receive nbr status
    #gol_cell_state{
        nbrs_answers = #{answer_count := AnswerCount, alive_count := AliveCount},
        nbrs_count = NbrsCount
    } = State,

    AnswerCount1 = AnswerCount + 1,
    Sum = status_to_int(StatusNbr) + AliveCount,

    if
        AnswerCount1 =:= NbrsCount ->

            NewStateName = fate(StateName, Sum),

            State1 = State#gol_cell_state{
                nbrs_answers = #{answer_count => 0, alive_count => 0}
            },

            {registered_name, MyName} = process_info(self(), registered_name),
            gol_demiurge ! {done, MyName, NewStateName},
            {next_state, NewStateName, State1};
        true ->
            State1 = State#gol_cell_state{
                nbrs_answers = #{answer_count => AnswerCount1, alive_count => Sum}
            },
            {keep_state, State1}
    end;

handle_event(_EventType, EventContent, _StateName, _State) ->
    gol_utils:log_info("Unsupported EventContent: ~p", [EventContent]),
    keep_state_and_data.


%%%===================================================================
%%% Internal functions
%%%===================================================================


fate(?STATUS_DEAD, Num) ->
    {Born, _Survive} = get(?PD_LIFE_RULE),
    fate(Num, Born);
fate(?STATUS_ALIVE, Num) ->
    {_Born, Survive} = get(?PD_LIFE_RULE),
    fate(Num, Survive);
fate(Num, List) ->
    case lists:member(Num, List) of
        true -> ?STATUS_ALIVE;
        false -> ?STATUS_DEAD
    end.

status_to_int(?STATUS_ALIVE) ->
    1;
status_to_int(?STATUS_DEAD) ->
    0;
status_to_int(_) ->
    0.


%%%===================================================================
%%% gen_statem callbacks 2
%%%===================================================================

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    handle_event_function. %% state_functions

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