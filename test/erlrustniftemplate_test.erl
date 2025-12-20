-module(erlrustniftemplate_test).
-include_lib("eunit/include/eunit.hrl").

hello_test() ->
    ?assertEqual(ok, erlrustniftemplate:hello()),
    ok.
