%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%
%% @doc This is the Abacus Server.
-module(abacus_gen_srv).
-behavoiur(gen_server).

%% Life-cycle API
-export([start/1,
         stop/1]).
%% Sync API
-export([addition/3,
         subtraction/3,
         multiplication/3,
         division/3]).
%% Async API
-export([async_addition/3,
         async_subtraction/3,
         async_multiplication/3,
         async_division/3,
         result_by_reference/2]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

%%====================================================================
%% API functions
%%====================================================================

start(Name) ->
    ServerName = {local, Name},
    CallbackModule = ?MODULE,
    InitArgs = [],
    Opts = [],
    gen_server:start(ServerName, CallbackModule, InitArgs, Opts).

stop(Name) ->
    gen_server:stop(Name).

addition(Name, X, Y) ->
    Request = {addition, X, Y},
    gen_server:call(Name, Request).

async_addition(Name, X, Y) ->
    Request = {addition, X, Y},
    Ref = make_ref(),
    gen_server:cast(Name, {Ref, Request}),
    {ok, Ref}.

subtraction(Name, X, Y) ->
    Request = {subtraction, X, Y},
    gen_server:call(Name, Request).

async_subtraction(Name, X, Y) ->
    Request = {subtraction, X, Y},
    Ref = make_ref(),
    gen_server:cast(Name, {Ref, Request}),
    {ok, Ref}.

multiplication(Name, X, Y) ->
    Request = {multiplication, X, Y},
    gen_server:call(Name, Request).

async_multiplication(Name, X, Y) ->
    Request = {multiplication, X, Y},
    Ref = make_ref(),
    gen_server:cast(Name, {Ref, Request}),
    {ok, Ref}.

division(Name, X, Y) ->
    Request = {division, X, Y},
    gen_server:call(Name, Request).

async_division(Name, X, Y) ->
    Request = {division, X, Y},
    Ref = make_ref(),
    gen_server:cast(Name, {Ref, Request}),
    {ok, Ref}.

result_by_reference(Name, Ref) ->
    Request = {result_by_reference, Ref},
    gen_server:call(Name, Request).

%%====================================================================
%% Callbacks
%%====================================================================

init(_) ->
    {ok, #{}}.

handle_call(Request, _From, State) ->
    Response = response(Request, State),
    {reply, {ok, Response}, State}.

handle_cast({Ref, Request}, State) ->
    Response = response(Request, State),
    NewState = store_response(Ref, Response, State),
    {noreply, NewState}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internals
%%====================================================================

response({addition, X, Y}, _State) ->
    abacus:addition(X, Y);
response({subtraction, X, Y}, _State) ->
    abacus:subtraction(X, Y);
response({multiplication, X, Y}, _State) ->
    abacus:multiplication(X, Y);
response({division, X, Y}, _State) ->
    abacus:division(X, Y);
response({result_by_reference, Ref}, State) ->
    retrieve_response(Ref, State).

store_response(Ref, Response, State) -> 
    State#{Ref => Response}.

retrieve_response(Ref, State) ->
    maps:get(Ref, State, unknown_reference).
