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

-export([
  extract_params/2,
  render/2,
  swaggerpath2module/2,
  swaggerpath_build_regex/1,
  swaggerpath_build_regex/2,
  to_atom/1,
  to_binary/1
]).

-export_type([params/0]).

-type key()      :: binary().
-type matches()  :: {integer(), integer()}.
-type pairs()    :: list({key(), value()}).
-type params()   :: list(list()).
-type path()     :: list().
-type value()    :: binary().

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
%   MP = swaggerpath_build_regex("/users/{userid}"),
%   {match, _} = re:run("/users/hello", MP).
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
%   MP = swaggerpath_build_regex("/users/{userid}", "/GET"),
%   {match, _} = re:run("/GET/users/hello", MP).
% @end
-spec swaggerpath_build_regex(string(), string()) -> re:mp().
swaggerpath_build_regex(SwaggerPath, Head) ->
  swaggerpath_build_regex(Head ++ SwaggerPath).

-spec extract_params(path(), matches()) -> params().
extract_params(Path, [_First | Matches]) ->
  [string:substr(
     binary_to_list(Path), Start + 1, Length) || {Start, Length} <- Matches].

-spec to_atom(binary() | list() | binary()) -> atom().
to_atom(Atom)   when is_atom(Atom)     -> Atom;
to_atom(List)   when is_list(List)     -> list_to_atom(List);
to_atom(Binary) when is_binary(Binary) -> to_atom(binary_to_list(Binary)).

-spec to_binary(binary() | list() | atom()) -> binary().
to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(List)   when is_list(List)     -> list_to_binary(List);
to_binary(Atom)   when is_atom(Atom)     -> to_binary(atom_to_list(Atom)).

-spec render(binary(), pairs()) -> binary().
render(Template, SubPair) ->
  lists:foldl(
    fun({K, V}, Acc) ->
        Pattern = << <<"{{">>/binary, K/binary, <<"}}">>/binary >>,
        re:replace(Acc, Pattern, V, [{return, binary}, global])
    end, Template, SubPair).
