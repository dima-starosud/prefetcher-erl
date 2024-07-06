%%%-------------------------------------------------------------------
%% @doc prefetcher top level supervisor.
%%%-------------------------------------------------------------------

-module(prefetcher_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link([]) ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, Endpoints} = application:get_env(prefetcher, endpoints),
    {ok, CowboyOpts} = application:get_env(prefetcher, cowboy_opts),

    SupFlags = #{},
    CowboyChildSpec = cowboy_child_spec(Endpoints, CowboyOpts),
    WorkerChildSpecs = lists:map(fun worker_child_spec/1, Endpoints),
    {ok, {SupFlags, [CowboyChildSpec | WorkerChildSpecs]}}.

cowboy_child_spec(Endpoints, CowboyOpts) ->
    Routes = [
        {<<"/api/", Id/binary>>, prefetcher_handler, [Id]}
     || #{id := Id} <- Endpoints
    ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Start = {
        cowboy,
        start_clear,
        [
            prefetcher_http_listener,
            CowboyOpts,
            #{env => #{dispatch => Dispatch}}
        ]
    },
    #{id => cowboy, start => Start}.

worker_child_spec(Endpoint = #{id := Id}) ->
    #{
        id => Id,
        start => {prefetcher_worker, start_link, [Endpoint]}
    }.
