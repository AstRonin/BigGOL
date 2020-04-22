%%%-------------------------------------------------------------------
%% @doc gol top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gol_sup).

-behaviour(supervisor).

-include("gol.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

    #{rows := Rows, cols := Cols, layers := Layers} = gol_utils:config(field),
    Field = #field{rows = Rows, cols = Cols, layers = Layers},

    gol_utils:log_info("Field: ~p", [Field]),
    ChildSpecs = create_cells(Rows - 1, Cols - 1, 0, Field, []),

    CellIds = [Id || #{id := Id} <- ChildSpecs],

    ChildSpecs1 = ChildSpecs ++ [#{id => gol_demiurge, start => {gol_demiurge, start_link, [CellIds]}}],

    gol_utils:log_info("Supervisor start with child: ~p", [ChildSpecs1]),
    {ok, {SupFlags, ChildSpecs1}}.

%%====================================================================
%% Internal functions
%%====================================================================


-spec create_cells(integer(), integer(), integer(), #field{}, list()) -> term().
create_cells(-1, _Col, _Layer, _Field, ChildSpecs) -> ChildSpecs;
create_cells(Row, Col, _Layer, Field, ChildSpecs) ->
    create_cells(Row - 1, Col, _Layer, Field, create_row(Row, Col, _Layer, Field, ChildSpecs)).

-spec create_row(integer(), integer(), integer(), #field{}, list()) -> term().
create_row(_Row, -1, _Layer, _Field, ChildSpecs) -> ChildSpecs;
create_row(Row, Col, Layer, Field, ChildSpecs) ->
    ChildSpecs1 = [create_cell_spec(#cell{row = Row, col = Col, layer = Layer}, Field) | ChildSpecs],
    create_row(Row, Col - 1, Layer, Field, ChildSpecs1).

-spec create_cell_spec(#cell{}, #field{}) -> term().
create_cell_spec(Cell, Field) ->
    Name = gol_utils:key(Cell),
    #{id => Name, start => {gol_cell, start_link, [Name, Cell, Field]}}.