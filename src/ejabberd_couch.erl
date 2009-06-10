-module(ejabberd_couch).

-author("tofu@collecta.com").

-export([init/1]).

init(Opts) ->
    %% set up and load ecouch
    application:set_env(?MODULE, host, gen_mod:get_opt(host, Opts, "127.0.0.1")),
    application:set_env(?MODULE, port, gen_mod:get_opt(port, Opts, "5984")),
    application:set_env(?MODULE, user, gen_mod:get_opt(user, Opts, none)),
    application:set_env(?MODULE, pass, gen_mod:get_opt(pass, Opts, none)),

    ok.



get_opt(Opt, Default) ->
    case application:get_env(?MODULE, Opt) of
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> Val;
                error -> Default
            end
        end.

