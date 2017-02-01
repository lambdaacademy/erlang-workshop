%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%
%% @doc This is the Abacus Supervisor.
-module(abacus_sup).
-behavoiur(supervisor).

%% Life-cycle API
-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

start_link(AbacusName) ->
    SupervisorName = {local, ?MODULE},
    CallbackModule = ?MODULE,
    InitArgs = [AbacusName],
    supervisor:start_link(SupervisorName, CallbackModule, InitArgs).

init([AbacusName]) ->
    SupFlags = #{intensity => 5, %% max 5 restarts
                 period => 10, %% in 10 seconds
                 strategy => one_for_one},
    AbacusStart = {abacus_gen_srv, start_link, [AbacusName]}, %% MFA
    Child = #{id => abacus_gen_srv,
              start => AbacusStart,
              restart => permanent,
              shutdown => 1000,
              type => worker,
              modules => [abacus_gen_srv]},
    Children = [Child],
    {ok, {SupFlags, Children}}.
