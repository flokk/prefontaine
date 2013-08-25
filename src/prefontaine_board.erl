-module(prefontaine_board).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, Conf) ->
  {ok, Req, Conf}.

handle(Req, Conf) ->
  {Method, Req2} = cowboy_req:method(Req),
  {Board, Req3} = cowboy_req:qs_val(<<"board">>, Req2),
  {ok, Req4} = reply(Method, Board, Conf, Req3),
  {ok, Req4, Conf}.

reply(<<"GET">>, undefined, _, Req) ->
  
  Data = [
    {<<"error">>, [
      {<<"message">>, <<"Missing board parameter">>}
    ]}
  ],

  cowboy_req:reply(400, [], jsx:encode(Data), Req);
reply(<<"GET">>, Board, {Size, C}, Req) ->
  URL = cowboy_base:resolve(<<"/boards?board=", Board/binary>>, Req),

  Query = [<<"ZREVRANGE">>, Board, <<"0">>, integer_to_binary(Size - 1), <<"WITHSCORES">>],

  {ok, Items} = eredis:q(C, Query),

  Data = [
    {<<"href">>, URL},
    {<<"root">>, [
      {<<"href">>, cowboy_base:resolve(<<"/">>, Req)}
    ]},
    {<<"top-items">>, group_item_score(Items, [])},
    {<<"set">>, [
      {<<"method">>, <<"POST">>},
      {<<"action">>, URL},
      {<<"input">>, [
        {<<"item">>, [
          {<<"type">>, <<"text">>}
        ]},
        {<<"score">>, [
          {<<"type">>, <<"number">>}
        ]}
      ]}
    ]}
  ],

  cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], jsx:encode(Data), Req);
reply(<<"POST">>, Board, {_, C} = Conf, Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  Data = jsx:decode(Body),
  Score = fast_key:get(<<"score">>, Data),
  Item = fast_key:get(<<"item">>, Data),
  {ok, _} = eredis:q(C, [<<"ZADD">>, Board, Score, Item]),
  reply(<<"GET">>, Board, Conf, Req2);
reply(_, _, _, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

group_item_score([], Acc) ->
  lists:reverse(Acc);
group_item_score([Item,Score|Rest], Acc) ->
  group_item_score(Rest, [[{<<"item">>, Item}, {<<"score">>, Score}]|Acc]).

terminate(_Reason, _Req, _State) ->
  ok.
