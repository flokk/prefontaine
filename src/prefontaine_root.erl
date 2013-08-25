-module(prefontaine_root).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {ok, Req3} = reply(Method, Req2),
  {ok, Req3, State}.

reply(<<"GET">>, Req) ->
  
  Data = [
    {<<"href">>, cowboy_base:resolve(<<"/">>, Req)},
    {<<"search">>, [
      {<<"method">>, <<"GET">>},
      {<<"action">>, cowboy_base:resolve(<<"/boards">>, Req)},
      {<<"input">>, [
        {<<"board">>, [
          {<<"type">>, <<"text">>}
        ]}
      ]}
    ]}
  ],

  cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], jsx:encode(Data), Req);
reply(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
  ok.
