%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%
%% @doc This is the Abacus Application.
-module(abacus_app).
-behavoiur(application).

-define(DEFAULT_ABACUS_NAME, server1).

%% Life-cycle API
-export([start/2,
         stop/1]).

start(_Type, _Args) ->
    Application = abacus,
    Key = server_name,
    ServerName = application:get_env(Application, Key,
                                     ?DEFAULT_ABACUS_NAME),
    abacus_sup:start_link(ServerName).

stop(_) ->
    ok.
