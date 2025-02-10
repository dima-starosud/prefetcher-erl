# prefetcher

The proxy, which prefetches certain 3rd party endpoints ahead of time and caches them for a particular period.

## Erlang/OTP version

The application requires Erlang/OTP 27.0 or higher.

## Build

    $ rebar3 compile

## Run

    $ rebar3 shell

After the application is started, one should be able to query the exchangerate endpoint (at the time of writing this is the only endpoint available):

    $ curl http://localhost:8080/api/exchangerates

    <exchangerates><row><exchangerate base_ccy="UAH" buy="43.45000" ccy="EUR" sale="44.45000"/></row><row><exchangerate base_ccy="UAH" buy="40.35000" ccy="USD" sale="40.95000"/></row></exchangerates>

If the resource wasn't updated in time (e.g. due to connectivity issues) the application will respond with 503 Service Temporarily Unavailable.

## Configuration

Many different 3rd party endpoints can be added later. For this one will have to implement another

The configuration consists of:
* `id :: binary` — the id of the endpoint, which is used as:
  * URL path `/api/{id}`;
  * key of the ETS table;
* `callback :: {M, F, A}` — the callback to call to get the fresh value corresponding to this 3rd party endpoint; may return `{ok, Value}` or `{error, Error}`;
* `ttl :: {Time, Unit}` — period after which the cache will expire;
* `update_period :: {Time, Unit}` — the period of updating the value, should be lower than `ttl` to ensure the value cached is always up to date;
* `retry_period :: {Time, Unit}` — the period to retry in case of `{error, Error}`.

Note, there is no configuration period, in case of the callback crashes, in this case it will be retried automatically by Erlang/OTP system, as a part of supervision tree.

## TODO

* Switch to `erlcron`.
* Use a dedicated logger instead of `io:format`.
* Add functions `-spec()`.
* Move the core of prefetcher to a separate application, which can be used for different sets of 3rd party endpoints.
