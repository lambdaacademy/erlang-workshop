%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%
%% @doc This is the Abacus Server.
-module(abacus_srv).

%% API exports
-export([start/1,
         stop/1,
         addition/3,
         subtraction/3,
         multiplication/3,
         division/3]).
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

subtraction(Name, X, Y) ->
    Request = {subtraction, X, Y},
    send_request(Name, Request).

multiplication(Name, X, Y) ->
    Request = {multiplication, X, Y},
    send_request(Name, Request).

division(Name, X, Y) ->
    Request = {division, X, Y},
    send_request(Name, Request).

%%====================================================================
%% Internal functions
%%====================================================================

init(Name) ->
    true = register(Name, self()),
    loop().

loop() ->
    receive
        {abacus_request, Caller, Request} ->
            Response = response(Request),
            Caller ! {abacus_response, {ok, Response}},
            loop();
        Message ->
            io:format("Got unknown message: ~p~n", [Message]),
            loop()
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

response({addition, X, Y}) ->
    abacus:addition(X, Y);
response({subtraction, X, Y}) ->
    abacus:subtraction(X, Y);
response({multiplication, X, Y}) ->
    abacus:multiplication(X, Y);
response({division, X, Y}) ->
    abacus:division(X, Y).
