%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2017 Leonardo Rossi
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
%%% @doc Swagger file.
%%% @end

-module(swagger_routerl_cowboy_v1_file_handler).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([to_text/2]).

-export([
    init/3,
    content_types_provided/2,
    rest_init/2
]).

%%%_ * API -------------------------------------------------------------

init(_Transport, _Req, _AppCtx) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, AppCtx) ->
	{ok, Req, AppCtx}.

content_types_provided(Req, AppCtx) ->
  {[{<<"text/html">>, to_text}], Req, AppCtx}.

to_text(ReqData, #{swagger_file := SwaggerFile}=Context) ->
  {SwaggerFile, ReqData, Context}.

