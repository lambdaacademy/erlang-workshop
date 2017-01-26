-module(abacus_tests).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    ?assertEqual(2+3, abacus:addition(2,3)).

sub_test() ->
    ?assertEqual(2-3, abacus:subtraction(2,3)).

mul_test() ->
    ?assertEqual(2*3, abacus:multiplication(2,3)).

div_test() ->
    ?assertEqual(2 div 3, abacus:division(2,3)).
