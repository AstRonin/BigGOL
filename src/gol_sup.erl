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
    #field{rows = Rows, cols = Cols} = Field = gol_utils:config(field),
    ChildSpecs = create_cells(Rows, Cols, 0, Field, []),

    CellIds = [Id || #{id => Id} <- ChildSpecs],

    ChildSpecs1 = [
        #{id => gol_demiurge, start => {gol_demiurge, start_link, [CellIds]}} |
        ChildSpecs
    ],
    gol_utils:info("Supervisor start with child: ~p", [ChildSpecs1]),
    {ok, {SupFlags, ChildSpecs1}}.

%%====================================================================
%% Internal functions
%%====================================================================



-spec create_cells(integer(), integer(), integer(), #field{}, list()) -> term().
create_cells(-1, _Col, _Layer, _Field, ChildSpecs) -> ChildSpecs;
create_cells(Row, Col, _Layer, Field, ChildSpecs) ->
    create_cells(Row - 1, Col, _Layer, Field, create_row(Row, Col, _Layer, Field, ChildSpecs)).
.

-spec create_row(integer(), integer(), integer(), #field{}, list()) -> term().
create_row(_Row, -1, _Layer, _Field, ChildSpecs) -> ChildSpecs;
create_row(Row, Col, Layer, Field, ChildSpecs) ->
    [create_cell_spec(#cell{row = Row, col = Col, layer = Layer}) | ChildSpecs].

-spec create_cell_spec(#cell{}) -> term().
create_cell_spec(Cell) ->
    Name = gol_utils:key(Cell),
    #{id => Name, start => {gol_cell, start_link, [Cell]}}.