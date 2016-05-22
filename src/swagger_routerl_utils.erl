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

-export([swaggerpath2module/2, swaggerpath_build_regex/1, swaggerpath_build_regex/2]).

% @doc Convert a swagger path into a concrete erlang module name.
%
% e.g. ws_users_userid = swaggerpath2module("ws_", "/users/{userid}").
% @end
-spec swaggerpath2module(string(), string()) -> atom().
swaggerpath2module(BaseName, PathConfig) ->
  Tokens = string:tokens(PathConfig, "/{}-"),
  list_to_atom(BaseName ++ string:join(Tokens, "_")).

% @doc Build the regex of a swagger path item.
%
% e.g.
%    MP = swaggerpath_build_regex("/users/{userid}"),
%    {match, _} = re:run("/users/hello", MP).
% @end
-spec swaggerpath_build_regex(string()) -> re:mp().
swaggerpath_build_regex(SwaggerPath) ->
  List = string:tokens(SwaggerPath, "/"),
  RegexList = lists:map(
    fun(El) ->
      case re:run(El, "^{.+}$") of
        {match, _} -> "([\\w|-]+)";  % extended version: ([^/]+)
        _Rest -> El
      end
    end, List),
  RegEx = "^/" ++ string:join(RegexList, "/") ++ "$",

  {ok, MP} = re:compile(RegEx),
  MP.

% @doc Do the same job of swaggerpath_build_regex/1, adding a default head to
% the compiled regex.
%
% e.g.
%   re:compile(
%     "^/GET/users/([\\w|-]+)$"
%   ) = swaggerpath_build_regex("/users/{userid}", "/GET").
% @end
-spec swaggerpath_build_regex(string(), string()) -> re:mp().
swaggerpath_build_regex(SwaggerPath, Head) ->
  swaggerpath_build_regex(Head ++ SwaggerPath).
