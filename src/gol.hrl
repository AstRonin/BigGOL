%%%-------------------------------------------------------------------
%%% @author Roman Shuplov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2020 0:13
%%%-------------------------------------------------------------------
-author("Roman Shuplov").

-include_lib("wx/include/wx.hrl").

-define(STATUS_ALIVE, alive).
-define(STATUS_DEAD, dead).

-record(field, {
    rows = 0 :: non_neg_integer(),
    cols = 0 :: non_neg_integer(),
    layers = 0 :: non_neg_integer()
}).

-record(cell, {
    row = 0 :: non_neg_integer(),
    col = 0 :: non_neg_integer(),
    layer = 0 :: non_neg_integer()
}).