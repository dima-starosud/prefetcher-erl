%%%-------------------------------------------------------------------
%% @doc Gen server module, which updates the cached value periodically.
%%%-------------------------------------------------------------------

-module(prefetcher_worker).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-include("include/prefetcher.hrl").

-record(state, {
    % storage key of the value produced by this worker
    id :: binary(),
    % callback, producing `{ok, prefetcher_response()} | {error, term()}`
    callback :: {atom(), atom(), list()},
    % milliseconds, period of time, the value is cached
    ttl :: pos_integer(),
    % milliseconds, defines the frequency of updating the value
    update_period :: pos_integer(),
    % milliseconds, time period in which the callback will be retried
    % in case of `{error, _}`
    retry_period :: pos_integer()
}).

start_link(RawState) ->
    gen_server:start_link(?MODULE, RawState, []).

init(#{
    id := Id,
    callback := Callback,
    ttl := Ttl,
    update_period := UpdatePeriod,
    retry_period := RetryPeriod
}) ->
    % Expected: Ttl > UpdatePeriod > RetryPeriod,
    % otherwise the value may expire before the fresh one is computed.
    % @todo Should we warn in case of ^^^ this requirement doesn't hold?
    % @todo In general, should we validate the configuration?
    [TtlMs, UpdatePeriodMs, RetryPeriodMs] = [
        erlang:convert_time_unit(Time, Unit, millisecond)
     || {Time, Unit} <- [Ttl, UpdatePeriod, RetryPeriod]
    ],
    State = #state{
        id = Id,
        callback = Callback,
        ttl = TtlMs,
        update_period = UpdatePeriodMs,
        retry_period = RetryPeriodMs
    },

    % Update immediately.
    self() ! update,
    {ok, State}.

% Returns the time of the next update.
update(#state{
    id = Id,
    callback = {M, F, A},
    ttl = Ttl,
    update_period = UpdatePeriod,
    retry_period = RetryPeriod
}) ->
    io:format("Updating the value of ~p...\n", [Id]),
    UpdateStart = erlang:monotonic_time(millisecond),
    % Try to execute the user-defined callback:
    case apply(M, F, A) of
        % If it's successful, update the value in the storage
        % and schedule next update using `update_period`.
        {ok, Value} ->
            % @todo Should we compute the expiration time relatively to the end of the call to `M:F(A)`?
            ExpiresAt = UpdateStart + Ttl,
            ok = prefetcher_storage:update(Id, ExpiresAt, Value),
            io:format("The value of ~p is successfully updated!\n", [Id]),
            % All is good, will recompute the value at `NextUpdateTime`.
            NextUpdateTime = UpdateStart + UpdatePeriod,
            NextUpdateTime;
        % In case of an error retry using another delay,
        % which is supposed to be shorter than the normal delay.
        {error, Error} ->
            io:format("Error recomputing ~p: ~p. Will retry...\n", [Id, Error]),
            % Retrying after a delay.
            erlang:monotonic_time(millisecond) + RetryPeriod
    end.

handle_info(update, State) ->
    NextUpdateTime = update(State),
    SendAfter = max(0, NextUpdateTime - erlang:monotonic_time(millisecond)),
    erlang:send_after(SendAfter, self(), update),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
