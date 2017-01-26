%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%
%% @doc This is the Abacus API module.
%%
%% It provides basic mathematical operations:
%% <li>addition,</li>
%% <li>subtraction,</li>
%% <li>multiplication,</li>
%% <li>division.</li>
-module(abacus).

%% API exports
-export([addition/2,
         subtraction/2,
         multiplication/2,
         division/2]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Adds two integers
%%
%% This adds `X' to `Y'.
addition(X, Y) ->
    X+Y.

%% @doc Subtracts two integers
%%
%% This subtracts `Y' from `X'.
subtraction(X, Y) ->
    X-Y.

%% @doc Multiplies two integers
%%
%% This multiplies `X' `Y' times.
multiplication(X, Y) ->
    X*Y.

%% @doc Divides two integers
%%
%% This multiplies divides `X' by `Y'.
division(X, Y) ->
    X div Y.

%%====================================================================
%% Internal functions
%%====================================================================
