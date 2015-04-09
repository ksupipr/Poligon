%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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

-module(ejabberd_auth_odbc).
-author('alexey@process-one.net').

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
	 store_type/0,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) ->
    ok.

plain_password_required() ->
    false.

store_type() ->
	plain.

%% @spec (User, Server, Password) -> true | false | {error, Error}
check_password(User, _Server, Password) ->
%%?INFO_MSG(" check_password : ~p~n", [User]),
%%?INFO_MSG(" check_password : ~p~n", [Password]),
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->

	    Passwd=hexstring(erlang:md5(LUser)),
	   (Passwd == Password) and (Password /= "")

    end.
%% @spec () -> List
hexstring(<<X:128/big-unsigned-integer>>) ->
lists:flatten(io_lib:format("~32.16.0b", [X])).

%% @spec (User, Server, Password, Digest, DigestGen) -> true | false | {error, Error}
check_password(User, _Server, Password, Digest, DigestGen) ->


    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	   %% Username = ejabberd_odbc:escape(LUser),
	    %%LServer = jlib:nameprep(Server),
	    Passwd=hexstring(erlang:md5(LUser)),

	    %%?INFO_MSG(" Password : ~p~n", [Password]),
	    %%?INFO_MSG(" userpass 2 : ~p~n", [Passwd]),
	    %%?INFO_MSG(" dig : ~p~n", [Digest]),
	    %%?INFO_MSG(" gen : ~p~n", [DigestGen(Passwd)]),
	    DigRes = if	 Digest /= "" ->
				     Digest == DigestGen(Passwd);
				 true ->
				     false
			     end,

		    if DigRes ->
			    true;
		       true ->
			(Passwd == Password) and (Password /= "")
		    end

    end.

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(_User, _Server, _Password) ->
 ok.


%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid}
try_register(_User, _Server, _Password) ->
{error, invalid_jid}.

dirty_get_registered_users() ->
ok.

get_vh_registered_users(_Server) ->
ok.

get_vh_registered_users(_Server, _Opts) ->
ok.

get_vh_registered_users_number(_Server) ->
0.

get_vh_registered_users_number(_Server, _Opts) ->
0.

get_password(User, _Server) ->
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	    %%Username = ejabberd_odbc:escape(LUser),
	    %%LServer = jlib:nameprep(Server),
	    hexstring(erlang:md5(LUser))
    end.

get_password_s(User, _Server) ->
    case jlib:nodeprep(User) of
	error ->
	    "";
	LUser ->

	    %%Username = ejabberd_odbc:escape(LUser),
	    %%LServer = jlib:nameprep(Server),
	    hexstring(erlang:md5(LUser))
    end.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(_User, _Server) ->
true.

%% @spec (User, Server) -> ok | error
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
remove_user(_User, _Server) ->
ok.

%% @spec (User, Server, Password) -> ok | error | not_exists | not_allowed
%% @doc Remove user if the provided password is correct.
remove_user(_User, _Server, _Password) ->
not_allowed.
