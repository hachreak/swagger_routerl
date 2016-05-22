%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2016 Leonardo Rossi
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%% @doc Utils.
%%% @end
-module(swagger_routerl_utils).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([swaggerpath2module/2]).

% @doc Convert a swagger path into a concrete erlang module name.
%
% e.g. ws_users_userid = swaggerpath2module("ws_", "/users/{userid}").
% @end
-spec swaggerpath2module(string(), string()) -> atom().
swaggerpath2module(BaseName, PathConfig) ->
  Tokens = string:tokens(PathConfig, "/{}-"),
  list_to_atom(BaseName ++ string:join(Tokens, "_")).
