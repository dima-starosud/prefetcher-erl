%%%-------------------------------------------------------------------
%% @doc Common prefetcher types.
%%%-------------------------------------------------------------------

%% The value, which is cached by the prefetcher for each endpoint.
%% Sent as is to the client, during the HTTP call.
-record(prefetcher_response, {
    headers :: #{binary() => iodata()},
    content :: binary()
}).
