%%%-------------------------------------------------------------------
%% @doc Prefetcher callback implementation for the exchange rate API.
%%%-------------------------------------------------------------------

-module(prefetcher_exchangerates).

-export([query/1]).

-include("include/prefetcher.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(HEADERS, #{<<"content-type">> => <<"application/xml">>}).

query(Url) ->
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RawJson}} ->
            io:format("Exchange Rates JSON: ~p\n", [RawJson]),
            RawXml = convert(RawJson),
            io:format("Exchange Rates XML: ~p\n", [RawXml]),
            Value = #prefetcher_response{headers = ?HEADERS, content = RawXml},
            {ok, Value};
        % If the external service is unavailable, trigger the retry.
        {ok, Error = {{_, Status, _}, _, _}} when 500 =< Status andalso Status < 600 ->
            {error, Error};
        % In case of connectivity issues, retry shortly.
        {error, Error = {failed_connect, _}} ->
            {error, Error}
        % @todo add more *known* failure conditions.
    end.

convert(RawJson) ->
    Json = json:decode(RawJson),
    XmlContent = json_to_xml_content(Json),
    RawXml = xmerl:export_content(XmlContent, xmerl_xml),
    iolist_to_binary(RawXml).

json_to_xml_content(JsonExchangeRates) ->
    [
        #xmlElement{
            name = exchangerates,
            content = [
                #xmlElement{
                    name = row,
                    content = [
                        #xmlElement{
                            name = exchangerate,
                            attributes = map_to_attributes(ExchangeRate)
                        }
                    ]
                }
             || ExchangeRate <- JsonExchangeRates
            ]
        }
    ].

map_to_attributes(Map) ->
    [
        #xmlAttribute{name = validate_key(Key), value = Value}
     || {Key, Value} <- maps:to_list(Map)
    ].

% @todo Allow extra fields, but fail in case of missing ones,
% see tests for details.
validate_key(<<"base_ccy">>) -> base_ccy;
validate_key(<<"ccy">>) -> ccy;
validate_key(<<"buy">>) -> buy;
validate_key(<<"sale">>) -> sale.

% TESTS

empty_convert_test() ->
    Json = <<"[ ]">>,
    Xml = convert(Json),
    Expected = <<"<exchangerates/>">>,
    ?assertEqual(Xml, Expected).

happy_path_convert_test() ->
    Json =
        <<
            "["
            "  {"
            "    \"base_ccy\": \"UAH\","
            "    \"ccy\":      \"USD\","
            "    \"buy\":  \"1.98\","
            "    \"sale\": \"2.02\""
            "  }"
            "]"
        >>,
    Xml = convert(Json),
    Expected =
        <<
            "<exchangerates>"
            "<row>"
            "<exchangerate base_ccy=\"UAH\" buy=\"1.98\" ccy=\"USD\" sale=\"2.02\"/>"
            "</row>"
            "</exchangerates>"
        >>,
    ?assertEqual(Xml, Expected).

% @todo Fix `convert`, so this test passes.
extra_field_convert_test() ->
    Json =
        <<
            "["
            "  {"
            "    \"_type\": \"ExchangeRate\","
            "    \"base_ccy\": \"UAH\","
            "    \"ccy\":      \"USD\","
            "    \"buy\":  \"1.98\","
            "    \"sale\": \"2.02\""
            "  }"
            "]"
        >>,
    Xml = convert(Json),
    Expected =
        <<
            "<exchangerates>"
            "<row>"
            "<exchangerate base_ccy=\"UAH\" buy=\"1.98\" ccy=\"USD\" sale=\"2.02\"/>"
            "</row>"
            "</exchangerates>"
        >>,
    ?assertEqual(Xml, Expected).

% @todo Fix `convert`, so this test passes.
missing_field_convert_test() ->
    Json =
        <<
            "["
            "  {"
            "    \"base_ccy\": \"UAH\","
            "    \"ccy\":      \"USD\","
            "    \"sale\": \"2.02\""
            "  }"
            "]"
        >>,
    ?assertThrow({missing_field, buy}, convert(Json)).
