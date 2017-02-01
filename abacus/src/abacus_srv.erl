%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%
%% @doc This is the Abacus Server.
-module(abacus_srv).

%% Server API
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
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start(Name) ->
    spawn(?MODULE, init, [Name]).

stop(Name) ->
    exit(whereis(Name), shutdown).

addition(Name, X, Y) ->
    Request = {addition, X, Y},
    send_request(Name, Request).

async_addition(Name, X, Y) ->
    Request = {addition, X, Y},
    send_async_request(Name, Request).

subtraction(Name, X, Y) ->
    Request = {subtraction, X, Y},
    send_request(Name, Request).

async_subtraction(Name, X, Y) ->
    Request = {subtraction, X, Y},
    send_async_request(Name, Request).

multiplication(Name, X, Y) ->
    Request = {multiplication, X, Y},
    send_request(Name, Request).

async_multiplication(Name, X, Y) ->
    Request = {multiplication, X, Y},
    send_async_request(Name, Request).

division(Name, X, Y) ->
    Request = {division, X, Y},
    send_request(Name, Request).

async_division(Name, X, Y) ->
    Request = {division, X, Y},
    send_async_request(Name, Request).

result_by_reference(Name, Ref) ->
    Request = {result_by_reference, Ref},
    send_request(Name, Request).

%%====================================================================
%% Internal functions
%%====================================================================

init(Name) ->
    true = register(Name, self()),
    loop(#{}).

loop(State) ->
    receive
        {abacus_request, Caller, Request} ->
            Response = response(Request, State),
            Caller ! {abacus_response, {ok, Response}},
            loop(State);
        {abacus_async_request, Ref, Request} ->
            Response = response(Request, State),
            NewState = store_response(Ref, Response, State),
            loop(NewState);
        Message ->
            io:format("Got unknown message: ~p~n", [Message]),
            loop(State)
    end.

send_request(Name, Request) ->
    Name ! {abacus_request, self(), Request},
    receive
        {abacus_response, {ok, Result}} ->
            Result;
        {abacus_response, {error, _} = Error} ->
            Error
    after 5000 ->
            {error, timeout}
    end.

send_async_request(Name, Request) ->
    Ref = make_ref(),
    Name ! {abacus_async_request, Ref, Request},
    Ref.

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
