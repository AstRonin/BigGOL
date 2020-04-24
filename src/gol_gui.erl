%%%-------------------------------------------------------------------
%%% @author Roman Shuplov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Apr 2020 16:16
%%%-------------------------------------------------------------------
-module(gol_gui).
-author("Roman Shuplov").

-behaviour(wx_object).

-include("gol.hrl").

-define(BRD, 10).
-define(ARC_R, 10).

%% API
-export([new/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(gol_gui_state, {frame, win, parent, board = [], pen, fonts = []}).

%%%===================================================================
%%% API
%%%===================================================================

new(Game) ->
    wx:new(),
    wx_object:start_link(?MODULE, [Game], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Game]) ->

%%    application_master:init(),

    {Frame, State} = wx:batch(fun() -> create_window() end),

    Game ! {gfx, self()},

    {Frame, State}.

create_window() ->
    Frame = wxFrame:new(wx:null(), -1, "Game Of Life", []),

    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    Top = wxBoxSizer:new(?wxHORIZONTAL),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Panel = wxPanel:new(Frame),

    SF = wxSizerFlags:new(),


    Win = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:setMaxSize(Win, {400, 400}),
    wxWindow:setMinSize(Win, {200, 200}),
    Pen = wxPen:new({0, 0, 0}, [{width, 3}]),
    Fs0 = [{Sz, wxFont:new(Sz, ?wxSWISS, ?wxNORMAL, ?wxNORMAL, [])} ||
        Sz <- [8, 9, 10, 11, 12, 13, 14, 16, 18, 20, 22, 24, 26, 28, 30, 34, 38, 42, 44, 46]],
    TestDC = wxMemoryDC:new(),
    Bitmap = wxBitmap:new(256, 256),
    wxMemoryDC:selectObject(TestDC, Bitmap),
    true = wxDC:isOk(TestDC),
    CW = fun({Sz, Font}, Acc) ->
        case wxFont:ok(Font) of
            true ->
                wxDC:setFont(TestDC, Font),
                CH = wxDC:getCharHeight(TestDC),
                [{CH, Sz, Font} | Acc];
            false ->
                Acc
        end
         end,
    Fs = lists:foldl(CW, [], Fs0),
    wxMemoryDC:destroy(TestDC),

%%    State = #gol_gui_state{},
    State = #gol_gui_state{frame = Frame, win = Win, board = [], pen = Pen, fonts = Fs},

%%    Size = wxWindow:getSize(Win),
%%    DC = wxPaintDC:new(Win),
%%    wxDC:destroyClippingRegion(DC),
%%    redraw(State),
%%    redraw(DC, Size, State),
%%    wxPaintDC:destroy(DC),

    wxSizer:add(MainSz, Win, wxSizerFlags:proportion(wxSizerFlags:expand(SF), 1)),
    wxWindow:setSizer(Panel, MainSz),
    wxSizer:fit(MainSz, Frame),
    wxSizer:setSizeHints(MainSz, Frame),
    wxWindow:show(Frame),

    redraw(State),

    {Frame, State}.

redraw(S = #gol_gui_state{win = Win}) ->
    DC0 = wxClientDC:new(Win),
    DC = wxBufferedDC:new(DC0),
    Size = wxWindow:getSize(Win),
    redraw(DC, Size, S),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.

redraw(DC, Size, S) ->
    wx:batch(fun() ->
        wxDC:setBackground(DC, ?wxWHITE_BRUSH),
        wxDC:clear(DC),
        BoxSz = draw_polygon(DC, Size, S),
        F = sel_font(BoxSz div 3, S#gol_gui_state.fonts)
%%        ,
%%        [draw_number(DC, F, BoxSz, Sq) || Sq <- S#gol_gui_state.board] %% wxDC:drawText(DC, integer_to_list(Num), {X+CW,Y+CH+1}),
             end).

sel_font(_BS, [{_H, _Sz, F}]) ->
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,_H, _BS]),
    F;
sel_font(BS, [{H, _Sz, F} | _]) when BS > (H + 6) ->
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,H, BS]),
    F;
sel_font(BS, [_ | Fs]) ->
    sel_font(BS, Fs).

%%draw_number(DC,F,Sz,#sq{key={R,C},val=Num,given=Bold,correct=Correct}) ->
%%    {X,Y} = get_coords(Sz,R-1,C-1),
%%    TBox = Sz div 3,
%%    if Bold ->
%%        wxFont:setWeight(F,?wxBOLD),
%%        wxDC:setTextForeground(DC,{0,0,0});
%%        Correct =:= false ->
%%            wxFont:setWeight(F,?wxNORMAL),
%%            wxDC:setTextForeground(DC,{255,40,40,255});
%%        true ->
%%            wxFont:setWeight(F,?wxNORMAL),
%%            wxDC:setTextForeground(DC,{50,50,100,255})
%%    end,
%%    wxDC:setFont(DC,F),
%%    CH = (TBox - wxDC:getCharHeight(DC)) div 2,
%%    CW = (TBox - wxDC:getCharWidth(DC)) div 2,
%%    wxDC:drawText(DC, integer_to_list(Num), {X+CW,Y+CH+1}),
%%    ok.

get_coords(Sz, R, C) ->
    TBox = Sz div 3,
    R1 = R div 3,
    R2 = R rem 3,
    C1 = C div 3,
    C2 = C rem 3,
    {?BRD + C1 * Sz + C2 * TBox,
        ?BRD + R1 * Sz + R2 * TBox}.

draw_polygon(DC, {W0, H0}, #gol_gui_state{pen = Pen}) ->
    BoxSz = getGeomSz(W0, H0),
    _BS = ?BRD + 3 * BoxSz,

    wxPen:setWidth(Pen, 3),
    wxPen:setColour(Pen, {0, 0, 0}),
    wxDC:setPen(DC, Pen),

    wxDC:drawRoundedRectangle(DC, {?BRD, ?BRD, 3 * BoxSz + 1, 3 * BoxSz + 1}, float(?ARC_R)),

    wxDC:drawPolygon(DC, [{?BRD+2, ?BRD+2}, {?BRD+20, ?BRD+20}, {?BRD+20, ?BRD+100}]),

    wxPen:setWidth(Pen, 20),
    wxDC:setPen(DC, Pen),
    wxDC:drawPoint(DC, {?BRD+30, ?BRD+30}),
    wxDC:drawPoint(DC, {?BRD+50, ?BRD+50}),
    wxDC:drawPoint(DC, {?BRD+100, ?BRD+20}),
    BoxSz
    .

draw_board(DC, {W0, H0}, #gol_gui_state{pen = Pen}) ->
    BoxSz = getGeomSz(W0, H0),
    BS = ?BRD + 3 * BoxSz,

    wxPen:setWidth(Pen, 3),
    wxPen:setColour(Pen, {0, 0, 0}),
    wxDC:setPen(DC, Pen),

    wxDC:drawRoundedRectangle(DC, {?BRD, ?BRD, 3 * BoxSz + 1, 3 * BoxSz + 1}, float(?ARC_R)),
    %% Testing DrawLines
    wxDC:drawLines(DC, [{?BRD + BoxSz, ?BRD}, {?BRD + BoxSz, BS}]),
    wxDC:drawLine(DC, {?BRD + BoxSz * 2, ?BRD}, {?BRD + BoxSz * 2, BS}),
    wxDC:drawLine(DC, {?BRD, ?BRD + BoxSz}, {BS, ?BRD + BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD + BoxSz * 2}, {BS, ?BRD + BoxSz * 2}),

    %% Draw inside lines
    wxPen:setWidth(Pen, 1),
    wxDC:setPen(DC, Pen),
    TBox = BoxSz div 3,
    wxDC:drawLine(DC, {?BRD + TBox, ?BRD}, {?BRD + TBox, BS}),
    wxDC:drawLine(DC, {?BRD + TBox * 2, ?BRD}, {?BRD + TBox * 2, BS}),
    wxDC:drawLine(DC, {?BRD + TBox + BoxSz, ?BRD}, {?BRD + TBox + BoxSz, BS}),
    wxDC:drawLine(DC, {?BRD + TBox * 2 + BoxSz, ?BRD}, {?BRD + TBox * 2 + BoxSz, BS}),
    wxDC:drawLine(DC, {?BRD + TBox + BoxSz * 2, ?BRD}, {?BRD + TBox + BoxSz * 2, BS}),
    wxDC:drawLine(DC, {?BRD + TBox * 2 + BoxSz * 2, ?BRD}, {?BRD + TBox * 2 + BoxSz * 2, BS}),
    %% Vert
    wxDC:drawLine(DC, {?BRD, ?BRD + TBox}, {BS, ?BRD + TBox}),
    wxDC:drawLine(DC, {?BRD, ?BRD + TBox * 2}, {BS, ?BRD + TBox * 2}),
    wxDC:drawLine(DC, {?BRD, ?BRD + TBox + BoxSz}, {BS, ?BRD + TBox + BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD + TBox * 2 + BoxSz}, {BS, ?BRD + TBox * 2 + BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD + TBox + BoxSz * 2}, {BS, ?BRD + TBox + BoxSz * 2}),
    wxDC:drawLine(DC, {?BRD, ?BRD + TBox * 2 + BoxSz * 2}, {BS, ?BRD + TBox * 2 + BoxSz * 2}),
    BoxSz.

getGeomSz(W, H) ->
    Small = if W < H -> W; true -> H end,
    (Small - 2 * ?BRD) div 3.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #gol_gui_state{}) ->
    {reply, Reply :: term(), NewState :: #gol_gui_state{}} |
    {reply, Reply :: term(), NewState :: #gol_gui_state{}, timeout() | hibernate} |
    {noreply, NewState :: #gol_gui_state{}} |
    {noreply, NewState :: #gol_gui_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #gol_gui_state{}} |
    {stop, Reason :: term(), NewState :: #gol_gui_state{}}).
handle_call(_Request, _From, State = #gol_gui_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #gol_gui_state{}) ->
    {noreply, NewState :: #gol_gui_state{}} |
    {noreply, NewState :: #gol_gui_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gol_gui_state{}}).
handle_cast(_Request, State = #gol_gui_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #gol_gui_state{}) ->
    {noreply, NewState :: #gol_gui_state{}} |
    {noreply, NewState :: #gol_gui_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gol_gui_state{}}).
handle_info(_Info, State = #gol_gui_state{}) ->
    {noreply, State}.



handle_event(#wx{event = #wxClose{}}, State = #gol_gui_state{frame = Frame}) ->
    io:format("~p Closing window ~n", [self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...", []),
    {stop, normal, State};
handle_event(Request, State) ->
    io:format("~p got event ~p ~n", [self(), Request]),
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #gol_gui_state{}) -> term()).
terminate(_Reason, _State = #gol_gui_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #gol_gui_state{},
    Extra :: term()) ->
    {ok, NewState :: #gol_gui_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #gol_gui_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
