%%%-------------------------------------------------------------------
%%% @author Roman Shuplov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2020 20:08
%%%-------------------------------------------------------------------
-module(gol_demiurge).
-author("Roman Shuplov").

-behaviour(gen_server).

-include("gol.hrl").

-define(MODE_FAST, mode_fast).
-define(MODE_TIME, mode_time).

%% API
-export([start_link/1]).

-export([seed_cells/1, clear_cells/0, run/0, run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(gol_demiurge_state, {
    cellIds = [] :: [atom()],
    timer = undefined :: tref(),
    mode = ?MODE_TIME :: ?MODE_TIME|?MODE_FAST,
    timeline = 1000 :: integer(),
    await_cells_done = 0 :: integer() %% length(cellIds)
%%    review_cell_done = 0 :: integer(),
%%    ready_for_next_tick = true :: boolean() %% true if review_cell_done =:= length(cellIds)
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link([atom()]) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(CellIds) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, CellIds, []).

seed_cells(Seed) ->
    gen_server:call(?SERVER, {seed_cells, Seed}).

clear_cells() ->
    gen_server:call(?SERVER, clear_cells).

run() ->
    gen_server:cast(?SERVER, {run, undefined}).
run(Num) ->
    gen_server:cast(?SERVER, {run, Num}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
%%-spec(init(Args :: term()) ->
%%    {ok, State :: #gol_demiurge_state{}} | {ok, State :: #gol_demiurge_state{}, timeout() | hibernate} |
%%    {stop, Reason :: term()} | ignore).
init(CellIds) ->
    Timeline = gol_utils:config(timeline),
    Mode = case Timeline of
               T when T > 0 -> ?MODE_TIME;
               _T -> ?MODE_FAST
           end,

    [gol_cell:find_neighbors(Id) || Id <- CellIds],
    gol_utils:info("Find neighbors: ~p", [CellIds]),

    [gol_cell:seed(gol_utils:key(Row, Col)) || {Row, Col} <- gol_utils:config(seed)],
    gol_utils:info("Auto Seed: ~p", [gol_utils:config(seed)]),

    State = #gol_demiurge_state{
        cellIds = CellIds,
        mode = Mode,
        timeline = Timeline
    },

    {ok, State}.



%% @private
%% @doc Handling call messages
%%-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
%%    State :: #gol_demiurge_state{}) ->
%%    {reply, Reply :: term(), NewState :: #gol_demiurge_state{}} |
%%    {reply, Reply :: term(), NewState :: #gol_demiurge_state{}, timeout() | hibernate} |
%%    {noreply, NewState :: #gol_demiurge_state{}} |
%%    {noreply, NewState :: #gol_demiurge_state{}, timeout() | hibernate} |
%%    {stop, Reason :: term(), Reply :: term(), NewState :: #gol_demiurge_state{}} |
%%    {stop, Reason :: term(), NewState :: #gol_demiurge_state{}}).
handle_call({seed_cells, Seed}, From, State) ->
    [gol_cell:seed(gol_utils:key(Row, Col)) || {Row, Col} <- Seed],
    gol_utils:info("Seed: ~p", [Seed]),
    {reply, ok, State};
handle_call(clear_cells, From, State) ->
    [gol_cell:clear(Id) || Id <- State#gol_demiurge_state.cellIds],
    gol_utils:info("Clear"),
    {reply, ok, State};
handle_call(stop, _From, State) ->
    timer:cancel(State#gol_demiurge_state.timer),
    {reply, ok, State#gol_demiurge_state{timer = undefined}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
%%-spec(handle_cast(Request :: term(), State :: #gol_demiurge_state{}) ->
%%    {noreply, NewState :: #gol_demiurge_state{}} |
%%    {noreply, NewState :: #gol_demiurge_state{}, timeout() | hibernate} |
%%    {stop, Reason :: term(), NewState :: #gol_demiurge_state{}}).
handle_cast({run, Num}, State) ->
    Num1 = case Num of
               N when is_integer(N) -> N;
               _ -> gol_utils:config(count_of_cycle)
           end,
    #gol_demiurge_state{cellIds = CellIds, mode = Mode, timeline = Timeline} = State,

    State1 = State#gol_demiurge_state{await_cells_done = 0},

    gol_utils:info("Run"),

    call_check_cell(CellIds),

    State2 = if
        Mode =:= ?MODE_TIME ->
            Timer = timer:send_interval(Timeline, state_timeout),
            State1#gol_demiurge_state{timer = Timer};
        true -> State1
    end,
    {noreply, State2};
handle_cast(_Request, State = #gol_demiurge_state{}) ->
    {noreply, State}.

call_check_cell(CellIds) ->
    TimeSnapshot = gol_utils:get_timestamp(),
    [gol_cell:check_status(Id, TimeSnapshot) || Id <- CellIds].

%% @private
%% @doc Handling all non call/cast messages
%%-spec(handle_info(Info :: timeout() | term(), State :: #gol_demiurge_state{}) ->
%%    {noreply, NewState :: #gol_demiurge_state{}} |
%%    {noreply, NewState :: #gol_demiurge_state{}, timeout() | hibernate} |
%%    {stop, Reason :: term(), NewState :: #gol_demiurge_state{}}).
handle_info(state_timeout, #gol_demiurge_state{await_cells_done = 0, cellIds = CellIds} = State) ->
    call_check_cell(CellIds),
    {noreply, State};
handle_info(state_timeout, State) ->
    {noreply, State};

handle_info({done, _Id}, State) ->
    Count = State#gol_demiurge_state.await_cells_done - 1,
    Mode = State#gol_demiurge_state.mode,
    NewCount = if
        Count =:= 0, Mode =:= ?MODE_FAST -> %% send new tick immediately
            CellIds = State#gol_demiurge_state.cellIds,
            call_check_cell(CellIds),
            length(CellIds);
        true -> 0
    end,
    State1 = State#gol_demiurge_state{await_cells_done = NewCount},
    {noreply, State1};
handle_info(Info, State) ->
    gol_utils:info("Unexpected Message Info: ~p", [Info]),
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
%%    State :: #gol_demiurge_state{}) -> term()).
terminate(_Reason, _State = #gol_demiurge_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #gol_demiurge_state{},
    Extra :: term()) ->
    {ok, NewState :: #gol_demiurge_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #gol_demiurge_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
