-module(ejabberd_couch).

-author("tofu@collecta.com").

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([init/1,
	 doc_delete/3,
	 doc_update/3,
	 doc_create/3,
	 doc_get/2
	]).

init(Opts) ->
    %% set up and load ecouch
    application:set_env(?MODULE, host, gen_mod:get_opt(host, Opts, "127.0.0.1")),
    application:set_env(?MODULE, port, gen_mod:get_opt(port, Opts, "5984")),
    application:set_env(?MODULE, user, gen_mod:get_opt(user, Opts, none)),
    application:set_env(?MODULE, pass, gen_mod:get_opt(pass, Opts, none)),
    ibrowse:start(),
    ok.

get_url(Name, Id) ->
    CouchUrl = "http://"++get_opt(host, "127.0.0.1")++":"++get_opt(port, "5984")++"/",
    CouchUrl ++ ibrowse_lib:url_encode(Name) ++ "/" ++ ibrowse_lib:url_encode(Id).
    
send_request(Url, Type, Doc) ->
    User = get_opt(user, none),
    Pass = get_opt(pass, none),
    Options = case User of 
		  none ->
		      [{content_type, rfc4627:mime_type()}];
	          _ ->
		      [{content_type, rfc4627:mime_type()},
		       {basic_auth, {User, Pass}}
		      ]
	      end, %% add mime type, username and password
    EncDoc = case Doc of 
		 "" ->
		     [];
		 _ ->
		     rfc4627:encode(Doc)
	     end,
    case catch ibrowse:send_req(Url,
				[],
                                Type, EncDoc,
				Options) of
        {ok, "200", _Headers, Payload} ->
	    rfc4627:decode(Payload);
        {ok, "201", _Headers, Payload} ->
	    rfc4627:decode(Payload);
        Error ->
            throw({error, storage_error, Error})
    end.

doc_delete(Name, Id, Rev) ->
    Url = get_url(Name, Id) ++ "?rev="++ibrowse_lib:url_encode(binary_to_list(Rev)),
    send_request(Url, delete, []).

doc_update(Name, Id, Value) ->
    Url = get_url(Name, Id),
    send_request(Url, put, Value).

doc_create(Name, Id, Value) ->
    Url = get_url(Name, Id),
    send_request(Url, put, Value).

doc_get(Name, Id) ->
    Url = get_url(Name, Id),
    send_request(Url, get, []).


get_opt(Opt, Default) ->
    case application:get_env(?MODULE, Opt) of
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> Val;
                error -> Default
            end
        end.

