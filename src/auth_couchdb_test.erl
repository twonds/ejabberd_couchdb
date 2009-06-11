%%%
%% auth_couchdb_test.erl
%%

-include_lib("eunit/include/eunit.hrl").

-module(auth_couchdb_test).


starttest() ->
    application:start(inets), %% may already be loaded
    application:set_env(ecouch, host, "127.0.0.1"),
    application:set_env(ecouch, port, "5984"),
    application:set_env(ecouch, user, none),
    application:set_env(ecouch, pass, none),
    application:start(ecouch),
    application:start(ibrowse),
    application:start(ejabberd),
    sha:start(), %% may already be started
    ejabberd_couch:init([]),
    ok.

stoptest() ->
    application:stop(inets),
    application:stop(ecouch),
    application:stop(ibrowse),
    application:stop(ejabberd),
    ok.


ejabberd_auth_couchdb_register_test() ->
    starttest(),
    ejabberd_auth_couchdb:remove_user("tofu","localhost"),
    {atomic, ok} = ejabberd_auth_couchdb:try_register("tofu","localhost","test"),
    true = ejabberd_auth_couchdb:is_user_exists("tofu","localhost"),
    stoptest().


ejabberd_auth_couchdb_set_password_test() ->
    starttest(),
    case ejabberd_auth_couchdb:is_user_exists("tofu","localhost") of
	true ->
	    ok;
	false ->
	    {atomic, ok} = ejabberd_auth_couchdb:try_register("tofu","localhost","test")
    end,
    ok = ejabberd_auth_couchdb:set_password("tofu","localhost","test123"),
    true = ejabberd_auth_couchdb:check_password("tofu","localhost","test123"),
    stoptest().

ejabberd_auth_couchdb_check_password_test() ->
    starttest(),
    case ejabberd_auth_couchdb:is_user_exists("tofu","localhost") of
	true ->
	    ok;
	false ->
	    {atomic, ok} = ejabberd_auth_couchdb:try_register("tofu","localhost","test123")
    end,
    %% test for true and false results
    false = ejabberd_auth_couchdb:check_password("tofu","localhost","test"),
    true = ejabberd_auth_couchdb:check_password("tofu","localhost","test123"),
    stoptest().
    
