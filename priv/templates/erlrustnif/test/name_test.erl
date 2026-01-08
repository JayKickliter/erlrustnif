-module({{name}}_test).
-include_lib("eunit/include/eunit.hrl").

hello_test() ->
    ?assertEqual(ok, {{name}}:hello()),
    ok.
