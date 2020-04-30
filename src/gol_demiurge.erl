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

-define(PD_CELL_IDS, cell_ids).
-define(PD_CELL_COUNT, cell_count).

%% API
-export([start_link/1]).

-export([seed_cells/1, clear_cells/0, get_alive/0, run/0, run/1, wait/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(gol_demiurge_state, {
%%    cellIds = [] :: [atom()],
    cycle_count = 0 :: non_neg_integer(),
    timer = undefined :: timer:tref(),
    mode = ?MODE_TIME :: ?MODE_TIME|?MODE_FAST,
    timeline = 1000 :: integer(),
    await_cells_done = 0 :: integer(), %% length(cellIds)
    alive_cells = [] :: [atom()],
    gfx = undefined :: wxWindow:wxWindow()
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

-spec get_alive() -> [atom()|pid()].
get_alive() ->
    gen_server:call(?SERVER, get_alive).

run() ->
    gen_server:cast(?SERVER, {run, undefined}).
run(Num) ->
    gen_server:cast(?SERVER, {run, Num}).

wait() ->
    gen_server:call(?SERVER, wait).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
%%-spec(init(Args :: term()) ->
%%    {ok, State :: #gol_demiurge_state{}} | {ok, State :: #gol_demiurge_state{}, timeout() | hibernate} |
%%    {stop, Reason :: term()} | ignore).
init(CellIds) ->

    process_flag(message_queue_data, off_heap),

    Timeline = gol_utils:config(timeline),
    Mode = case Timeline of
               T when T > 0 -> ?MODE_TIME;
               _T -> ?MODE_FAST
           end,

%%    [gol_cell:find_neighbors(Id) || Id <- CellIds],
%%    gol_utils:log_info("Find neighbors: ~p", [CellIds]),
%%    find_neighbors_iterator(gb_sets:iterator(CellIds)),
    Iter = gb_sets:iterator(CellIds),
    spawn(fun() -> find_neighbors_iterator(Iter) end),

    AliveCellsTuple = gol_utils:config(seed),

    [gol_cell:seed(gol_utils:key(Row, Col)) || {Row, Col} <- AliveCellsTuple],

    gol_utils:log_info("Auto Seed: ~p", [gol_utils:config(seed)]),

    put(?PD_CELL_IDS, CellIds),
    put(?PD_CELL_COUNT, gb_sets:size(CellIds)),

    GFX = case gol_utils:config(gui) of
              G when G == true; G == 1 -> start_gui();
              _ -> undefined
          end,

    State = #gol_demiurge_state{
        mode = Mode,
        timeline = Timeline,
        gfx = GFX
    },

    send_alive_cells_to_gui(State, [{Row, Col, 0} || {Row, Col} <- AliveCellsTuple]),

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
handle_call({seed_cells, Seed}, _From, State) ->
    [gol_cell:seed(gol_utils:key(Row, Col)) || {Row, Col} <- Seed],
    send_alive_cells_to_gui(State, [{Row, Col, 0} || {Row, Col} <- Seed]),
    gol_utils:log_info("Seed: ~p", [Seed]),
    {reply, ok, State};

handle_call(clear_cells, _From, State) ->
%%    [gol_cell:clear(Id) || Id <- get(?PD_CELL_IDS)],
    Iter = gb_sets:iterator(get(?PD_CELL_IDS)),
    spawn(fun() -> clear_cells_iterator(Iter) end),
    gol_utils:log_info("Clear"),
    {reply, ok, State};

handle_call(get_alive, _From, State) ->
    Ids = find_cell_alive(),
    {reply, Ids, State};

handle_call(wait, _From, State) ->
    State1 = stop_timer(State),
    {reply, ok, State1};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
%%-spec(handle_cast(Request :: term(), State :: #gol_demiurge_state{}) ->
%%    {noreply, NewState :: #gol_demiurge_state{}} |
%%    {noreply, NewState :: #gol_demiurge_state{}, timeout() | hibernate} |
%%    {stop, Reason :: term(), NewState :: #gol_demiurge_state{}}).
handle_cast({run, Num}, #gol_demiurge_state{mode = Mode, timeline = Timeline} = State) ->
    Num1 =
        if
            is_integer(Num) -> Num;
            true -> gol_utils:config(cycle_count)
        end,

    State1 = State#gol_demiurge_state{cycle_count = Num1},

    gol_utils:log_info("Run"),

    State2 =
        if
            Mode =:= ?MODE_TIME -> %% Start by timer
                {ok, Timer} = timer:send_interval(Timeline, state_timeout),
                State1#gol_demiurge_state{timer = Timer};
            true -> %% Start by can
                handle_new_request(State1)
        end,
    {noreply, State2};

handle_cast(Request, State) ->
    gol_utils:log_warning("Cast unknown request: ~p", [Request]),
    {noreply, State}.


%% @private
%% @doc Handling all non call/cast messages
%%-spec(handle_info(Info :: timeout() | term(), State :: #gol_demiurge_state{}) ->
%%    {noreply, NewState :: #gol_demiurge_state{}} |
%%    {noreply, NewState :: #gol_demiurge_state{}, timeout() | hibernate} |
%%    {stop, Reason :: term(), NewState :: #gol_demiurge_state{}}).
handle_info(state_timeout, State) ->
    State1 = handle_new_request(State),
    {noreply, State1};

handle_info({done, Id, CellStateName}, #gol_demiurge_state{
    mode = Mode,
    await_cells_done = Count,
    alive_cells = AliveCells
} = State) ->
%%        gol_utils:log_info("done, Count: ~p", [Count]),
    AliveCells1 =
        if
            CellStateName =:= ?STATUS_ALIVE ->
%%                AliveCells;
                [Id | AliveCells]; %%%%%%%%%%%%%%%%%%%%% Revert !!!!!
            true ->
                AliveCells
        end,

    State2 =
        if
            Count =:= 1 ->
                State1 = State#gol_demiurge_state{alive_cells = []},
                send_alive_cells_to_gui(State1, [gol_utils:unkey(N) || N <- AliveCells1]),
                State1;
            true ->
                State#gol_demiurge_state{await_cells_done = Count - 1, alive_cells = AliveCells1}
        end,

    State0 =
        if
            Mode =:= ?MODE_FAST, Count =:= 1 -> handle_new_request(State2);
            true -> State2
        end,

    {noreply, State0};

handle_info(Info, State) ->
    gol_utils:log_warning("Unexpected Message Info: ~p", [Info]),
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

-spec start_gui() -> wxWindow:wxWindow().
start_gui() ->
    gol_gui:new().

-spec send_alive_cells_to_gui(#gol_demiurge_state{}, [{non_neg_integer(), non_neg_integer(), non_neg_integer()}]) -> ok.
send_alive_cells_to_gui(State, AliveCells) ->
    case State#gol_demiurge_state.gfx of
        undefined -> ok;
        GFX -> gol_gui:set_alive(GFX, AliveCells)
    end.

handle_new_request(#gol_demiurge_state{cycle_count = 0} = State) ->
    gol_utils:log_info("Finish!"),
    State1 = stop_timer(State),

    Ids = find_cell_alive(),
    gol_utils:log_info("Alive: ~p", [Ids]),

    Info = process_info(self()),

    Info1 = [{K, V} || {K, V} <- Info, K /= dictionary],

    gol_utils:log_info("Demiurge Bites: ~p", [Info1]), %% 45 238 556, 61 163 004

    State1#gol_demiurge_state{await_cells_done = 0};
handle_new_request(#gol_demiurge_state{cycle_count = CycleCount} = State) ->

%%    Ids = find_cell_alive(),
%%    gol_utils:log_info("Alive: ~p", [Ids]),

    gol_utils:log_info("Next Cycle: ~p", [CycleCount]),

    TimeSnapshot = list_to_atom(integer_to_list(gol_utils:get_timestamp()) ++ "." ++ integer_to_list(CycleCount)),

%%    gol_utils:log_info("Next Cycle: ~p, TimeSnapshot: ~p", [CycleCount, TimeSnapshot]),

%%    [gol_cell:check_status(Id, TimeSnapshot) || Id <- get(?PD_CELL_IDS)],
%%    check_status_iterator(gb_sets:iterator(get(?PD_CELL_IDS)), TimeSnapshot),
    Iter = gb_sets:iterator(get(?PD_CELL_IDS)),
    spawn(fun() -> check_status_iterator(Iter, TimeSnapshot) end),

    State#gol_demiurge_state{
        cycle_count = CycleCount - 1,
        await_cells_done = get(?PD_CELL_COUNT)
    }.

find_neighbors_iterator(Iter) ->
    process_flag(message_queue_data, off_heap),
    case gb_sets:next(Iter) of
        {Id, Iter2} ->
            gol_cell:find_neighbors(Id),
            find_neighbors_iterator(Iter2);
        none -> ok
    end.

check_status_iterator(Iter, TimeSnapshot) ->
    case gb_sets:next(Iter) of
        {Id, Iter2} ->
            gol_cell:check_status(Id, TimeSnapshot),
            check_status_iterator(Iter2, TimeSnapshot);
        none -> ok
    end.

find_cell_alive_iterator(Iter, Acc) ->
    case gb_sets:next(Iter) of
        {Id, Iter2} ->
            Acc1 = [{Id, gol_cell:is_alive(Id)} | Acc],
            find_cell_alive_iterator(Iter2, Acc1);
        none -> Acc
    end.

clear_cells_iterator(Iter) ->
    case gb_sets:next(Iter) of
        {Id, Iter2} ->
            gol_cell:clear(Id),
            clear_cells_iterator(Iter2);
        none -> ok
    end.


find_cell_alive() ->
    Iter = gb_sets:iterator(get(?PD_CELL_IDS)),
    [Id ||
        {Id, IsAlive} <- find_cell_alive_iterator(Iter, []),
        IsAlive =:= true
    ]
.





-spec stop_timer(#gol_demiurge_state{}) -> #gol_demiurge_state{}.
stop_timer(State) ->
    timer:cancel(State#gol_demiurge_state.timer),
    State#gol_demiurge_state{timer = undefined}.
