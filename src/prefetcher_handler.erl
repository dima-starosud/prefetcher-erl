%%%-------------------------------------------------------------------
%% @doc Cowboy handler of the prefetcher app.
%%%-------------------------------------------------------------------

-module(prefetcher_handler).

-export([init/2]).

-include("include/prefetcher.hrl").

init(Req0, [Id]) ->
    Req1 = maybe
        % Allow only method GET, return 405 otherwise.
        {_, <<"GET">>}   ?= {method, cowboy_req:method(Req0)},
        % If the value wasn't updated for a while, respond with 503.
        {_, {ok, Value}} ?= {lookup, prefetcher_storage:lookup(Id)},

        #prefetcher_response{headers = Headers, content = Content} = Value,
        cowboy_req:reply(200, Headers, Content, Req0)
    else
        {method, _} -> cowboy_req:reply(405, Req0);
        {lookup, _} -> cowboy_req:reply(503, #{}, "Service Temporarily Unavailable", Req0)
    end,

    {ok, Req1, []}.
