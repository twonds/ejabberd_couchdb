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
    ok = application:start(ecouch),
    sha:start(),
    ok.

plain_password_required() ->
    false.

%% @spec (User, Server, Password) -> true | false | {error, Error}
check_password(User, Server, Password) ->
    Jid = string:join([User, "@", Server], ""),
    CheckPass = sha:sha(Password),
    case catch ecouch:doc_get("users", Jid) of
	{ok, {obj, UserObj}} ->
	    case UserObj of 
		[{"password",Password},_] ->
		    ?INFO_MSG("User ~p~n",[UserObj]),
		    true;
		_ ->
		    false
	    end;
	{'EXIT', Error} ->
	    {error, Error};
	_ ->
	    false
    end.


%% @spec (User, Server, Password, StreamID, Digest) -> true | false | {error, Error}
check_password(User, Server, Password, StreamID, Digest) ->
    Jid = string:join([User, "@", Server], ""),
    {ok, {obj, [UserObj]}} = ecouch:doc_get("users", Jid),
    true.

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(User, Server, Password) ->
    Jid = string:join([User, "@", Server], ""),
    {ok, {obj, [UserObj]}} = ecouch:doc_get("users", Jid),

    false.


%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid}
try_register(User, Server, Password) ->
    Jid = string:join([User, "@", Server], ""),
    {ok, {obj, [UserObj]}} = ecouch:doc_get("users", Jid),
    {atomic, ok}.

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(couchdb),
    lists:flatmap(
      fun(Server) ->
	      get_vh_registered_users(Server)
      end, Servers).

get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    case catch list_users(LServer) of
	{selected, ["username"], Res} ->
	    [{U, LServer} || {U} <- Res];
	_ ->
	    []
    end.

get_vh_registered_users(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch list_users(LServer, Opts) of
	{selected, ["username"], Res} ->
	    [{U, LServer} || {U} <- Res];
	_ ->
	    []
    end.

get_vh_registered_users_number(Server) ->
    LServer = jlib:nameprep(Server),
    case catch users_number(LServer) of
	{selected, [_], [{Res}]} ->
	    list_to_integer(Res);
	_ ->
	    0
    end.

get_vh_registered_users_number(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch users_number(LServer, Opts) of
	{selected, [_], [{Res}]} ->
	    list_to_integer(Res);
	_Other ->
	    0
    end.

get_password(User, Server) ->
    false.

get_password_s(User, Server) ->
    "".

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    Jid = string:concat(User, "@", Server),
    case  ecouch:doc_get("users", Jid) of
	{ok, {obj, UserObj}} ->
	    true;
	_ ->
	    false
    end.

%% @spec (User, Server) -> ok | error
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
remove_user(User, Server) ->
    ok.

%% @spec (User, Server, Password) -> ok | error | not_exists | not_allowed
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, Password) ->
    ok.

%%%
%% private functions
%%

users_number(_Server) ->
    0.

users_number(_Server, _Opts) ->
    0.

list_users(_Server) ->
    [].


list_users(_Server, _Opts) ->
    [].

