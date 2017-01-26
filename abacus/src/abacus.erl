-module(abacus).

%% API exports
-export([addition/2,
         subtraction/2,
         multiplication/2,
         division/2]).

%%====================================================================
%% API functions
%%====================================================================

addition(X, Y) ->
    X+Y.

subtraction(X, Y) ->
    X-Y.

multiplication(X, Y) ->
    X*Y.

division(X, Y) ->
    X div Y.

%%====================================================================
%% Internal functions
%%====================================================================
