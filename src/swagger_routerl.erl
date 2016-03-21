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
%%% @doc Load the swagger file.
%%% @end
-module(swagger_routerl).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

%% API exports
-export([init/0, load/1]).

-export_type([appctx/0, filename/0, yaml/0]).

-type appctx()   :: ok.
-type filename() :: binary().
-type yaml()     :: list({string(), term()}).

%%====================================================================
%% API functions
%%====================================================================

-spec init() -> appctx().
init() ->
  application:start(yamerl).

-spec load(filename()) -> yaml().
load(Filename) ->
  [File] = yamerl:decode_file(Filename),
  File.

%%====================================================================
%% Internal functions
%%====================================================================
