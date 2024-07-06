%%%-------------------------------------------------------------------
%% @doc Prefetcher cache abstraction.
%% Allows to store the value with an expiration time,
%% and retrieve it later on.
%%%-------------------------------------------------------------------

-module(prefetcher_storage).

-export([create_link/0, update/3, lookup/1]).

-define(TABLE, ?MODULE).

-include("include/prefetcher.hrl").

-record(entry, {
    key :: binary(),
    value :: prefetcher_response,
    % note, uses `erlang:monotonic_time(millisecond)`
    expires_at :: integer()
}).

create_link() ->
    % Should we create a dedicated process owning this table instead?
    % Won't it become a bottleneck in case we have many recourses to prefetch?
    % Another option, is to create single-row-table per 3rd party endpoint?
    ets:new(?TABLE, [set, public, named_table, {keypos, #entry.key}]),
    ok.

update(Key, ExpiresAt, Value0 = #prefetcher_response{content = Content0}) ->
    % Ensure we do not copy much data during request handling.
    Content = iolist_to_binary(Content0),
    Value = Value0#prefetcher_response{content = Content},
    true = ets:insert(?TABLE, #entry{key = Key, value = Value, expires_at = ExpiresAt}),
    ok.

lookup(Key) ->
    maybe
        [Entry] ?= ets:lookup(?TABLE, Key),
        #entry{value = Value, expires_at = ExpiresAt} = Entry,
        true ?= erlang:monotonic_time(millisecond) =< ExpiresAt,
        {ok, Value}
    else
        _ -> undefined
    end.
