%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_couchdb.erl
%%% Author  : Christopher Zorn <tofu@thetofu.com>
%%% Purpose : Authentification via CouchDB
%%% Created : 04 April 2009 by Christopher "tofu" Zorn
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
%%%
%%% ejabberd_auth_couchdb, Copyright (C) 2009   Stanziq, Inc.
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_couchdb).
-author('tofu@thetofu.com').

%% External exports
-export([start/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

-define(COUCHDB_DBNAME, "users").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    Opts = ejabberd_config:get_local_option({couchdb_options, Host}),
    %% set up and load ecouch
    application:start(inets), %% may already be loaded
    application:set_env(ecouch, host, gen_mod:get_opt(host, Opts, "127.0.0.1")),
    application:set_env(ecouch, port, gen_mod:get_opt(port, Opts, "5984")),
    application:set_env(ecouch, user, gen_mod:get_opt(user, Opts, none)),
    application:set_env(ecouch, pass, gen_mod:get_opt(pass, Opts, none)),
    application:start(ecouch), %% may already be loaded
    sha:start(),
    ok.
%% ----------------------------
%% plain_password_required
%% 
%% tells ejabbered if a plain text password is required with this auth module.
plain_password_required() ->
    true.
%% ----------------------------
%% check_password
%% @spec (User, Server, Password) -> true | false | {error, Error}
check_password(User, Server, Password) ->
    Jid = string:join([User, "@", Server], ""),
    case catch get_user(Jid) of
	{ok, UserObj} ->
	    case get_obj_attr("password",UserObj) of
		{"password",UPassword} ->
		    CheckPass = sha:sha(Password),
		    if is_list(UPassword) ->
			    (CheckPass == UPassword);
		       is_binary(UPassword) ->
			    (CheckPass == binary_to_list(UPassword));
		       true ->
			    false
		    end;
		_ ->
		    false
	    end;
	{'EXIT', Error} ->
	    {error, Error};
	_ ->
	    false
    end.


%% @spec (User, Server, Password, StreamID, Digest) -> true | false | {error, Error}
check_password(User, Server, Password, _StreamID, _Digest) ->
    check_password(User, Server, Password).

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(User, Server, Password) ->
    Jid = string:join([User, "@", Server], ""),
    case get_user(Jid) of
	{ok, UserObj} ->
	    CheckPass = sha:sha(Password),
	    NewUser = {obj, [{"_id", list_to_binary(Jid)},
			     get_obj_attr("_rev", UserObj),
			     get_obj_attr("email", UserObj), 
			     {"password", CheckPass}
			    ]},
	    {ok,{obj,[{"ok",true},_,_]}} = ecouch:doc_update(?COUCHDB_DBNAME, Jid, NewUser),
	    ok;
	null ->
	    {error, invalid_jid};
	_E ->
	    ?INFO_MSG("Error ~p", [_E]),
	    {error, invalid_jid}
    end.

%% -------------------------------------
%% try_register
%% ejabberd api function to attempt a user registration.
%%
%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid}
try_register(User, Server, Password) ->
    Jid = string:join([User, "@", Server], ""),
    case get_user(Jid) of
	{ok, _} ->
	    {atomic, exists};
	_ ->
	    CheckPass = sha:sha(Password),
	    NewUser = {obj, [{"password", CheckPass}, {"email", null}]},
	    {ok,{obj, [{"ok",true},_,_]}} = ecouch:doc_create(?COUCHDB_DBNAME, Jid, NewUser),
	    {atomic, ok}
    end.
%% -------------------------------------
%% dirty_get_registered_users
%% NOTE: not implemented
dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(couchdb),
    lists:flatmap(
      fun(Server) ->
	      get_vh_registered_users(Server)
      end, Servers).
%% -------------------------------
%% get_vh_registered_users
%% NOTE: not implemented
get_vh_registered_users(Server) ->
    get_vh_registered_users(Server, none).

get_vh_registered_users(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch list_users(LServer, Opts) of
	{selected, ["username"], Res} ->
	    [{U, LServer} || {U} <- Res];
	_ ->
	    []
    end.

%% -------------------------------
%% get_vh_registered_users_number
%% NOTE: not implemented
get_vh_registered_users_number(Server) ->
    get_vh_registered_users_number(Server, none).

get_vh_registered_users_number(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch users_number(LServer, Opts) of
	{selected, [_], [{Res}]} ->
	    list_to_integer(Res);
	_Other ->
	    0
    end.
%% get password is not implemented and returns a false
%% this is required in the auth api
get_password(_User, _Server) ->
    false.
%% get password_s is not implemented and returns a string(list)
%% this is required in the auth api
get_password_s(_User, _Server) ->
    "".
%% -------------------------------
%% is_user_exists
%% needed for ejabberd's auth api to check if a user exists.
%%
%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    Jid = string:join([User, "@", Server], ""),
    case get_user(Jid) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

%% @spec (User, Server) -> ok | error
%% @doc Remove user.
remove_user(User, Server) ->
    Jid = string:join([User, "@", Server], ""),
    case get_user(Jid) of
	{ok, UObj} ->
	    case  get_obj_attr("_rev", UObj) of
		{"_rev", Rev} ->
		    remove_db_user(Jid, Rev);
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

%% @spec (User, Server, Password) -> ok | error | not_exists | not_allowed
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, Password) ->
    Jid = string:join([User, "@", Server], ""),
    case get_user(Jid) of
	{ok, UObj} ->
	    case  get_obj_attr("_rev", UObj) of
		{"_rev", Rev} ->
		    %% check password
		    Remove = case get_obj_attr("password",UObj) of
				 {"password",UPassword} ->
				     CheckPass = sha:sha(Password),
				     if is_list(UPassword) ->
					     (CheckPass == UPassword);
					is_binary(UPassword) ->
					     (CheckPass == binary_to_list(UPassword));
					true ->
					     false
				     end;
				 _ ->
				     false
			     end,
		    if (Remove == true) ->
			    remove_db_user(Jid, Rev);
		       true ->
			    not_allowed
		    end;
		_ ->
		    error
	    end;
	_R ->
	    not_exists
    end.

%%%
%% private functions
%%

%% -------------------------
%% get a user document from couchdb
get_user(Jid) ->
    case catch ecouch:doc_get(?COUCHDB_DBNAME, Jid) of
	{ok, {obj, [{"error", _Error},{"reason",_Reason}]}} ->
	    %% error is usually a not found
	    null;
	{ok, {obj, UserObj}} ->
	    {ok, UserObj};
	Error ->
	    ?INFO_MSG("AUTH: Error in connection to couch ~p",[Error]),
	    null
    end.
%% -------------------------
%% get a json object value using a key
%%
get_obj_attr(_Key, []) ->
    null;
get_obj_attr(Key, [H|Obj]) ->
    {HKey,_} = H,
    if (HKey == Key) ->
	    H;
       true ->
	    get_obj_attr(Key, Obj)
    end.
%% --------------------------
%% remove a user from couchdb
remove_db_user(Jid, Rev) ->
    catch ecouch:doc_delete(?COUCHDB_DBNAME, Jid, Rev).

%% below are stub functions used in unimplemented functions above.
users_number(_Server) ->
    0.

users_number(_Server, _Opts) ->
    0.


list_users(_Server, _Opts) ->
    [].

