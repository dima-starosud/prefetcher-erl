%%%-------------------------------------------------------------------
%% @doc prefetcher application interface.
%%%-------------------------------------------------------------------

-module(prefetcher_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    % @todo Probably, not the best idea to link the table to the
    % top level application process.
    ok = prefetcher_storage:create_link(),
    prefetcher_sup:start_link([]).

stop(_State) ->
    ok.
