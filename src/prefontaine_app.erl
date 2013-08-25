%% @private
-module(prefontaine_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->  
  BoardSize = simple_env:get_integer("BOARD_SIZE", 10),

  {ok, C} = case simple_env:get("REDIS_URL") of
    undefined ->
      eredis:start_link();
    RedisUrl ->
      {ok, {redis, Creds, Host, Port, _, []}} = http_uri:parse(RedisUrl),
      [_, Password] = binary:split(list_to_binary(Creds), <<":">>),
      eredis:start_link(Host, Port, undefined, binary_to_list(Password))
  end,

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", prefontaine_root, []},
      {"/boards", prefontaine_board, {BoardSize, C}}
    ]}
  ]),

  {ok, _} = cowboy:start_http(http, 100, [{
    port, simple_env:get_integer("PORT", 8080)}
  ], [
    {env, [{dispatch, Dispatch}]},
    {middlewares, [
      cowboy_base,
      cowboy_router,
      cowboy_handler
    ]}
  ]),

  prefontaine_sup:start_link().

stop(_State) ->
  ok.
