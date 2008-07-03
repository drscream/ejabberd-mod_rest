%%%-------------------------------------------------------------------
%%% File    : mod_rest.erl
%%% Author  : Nolan Eakins <sneakin@semanticgap.com>
%%% Purpose : Provide an HTTP interface to POST stanzas into ejabberd
%%%
%%% Copyright (C) 2008 Nolan Eakins
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
%%%-------------------------------------------------------------------

-module(mod_rest).
-author('sneakin@semanticgap.com').

-behavior(gen_mod).

-export([start/2,
	 stop/1,
	 process/2
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

start(_Host, _Opts) ->
    ?DEBUG("Starting: ~p ~p", [_Host, _Opts]),
    RESTSupervisor = {
      ejabberd_mod_rest_sup, {
	ejabberd_tmp_sup, start_link,
	[ejabberd_mod_rest_sup, ejabberd_mod_rest]
       },
      permanent,
      infinity,
      supervisor,
      [ejabberd_tmp_sup]
     },
    case supervisor:start_child(ejabberd_sup, RESTSupervisor) of
	{ok, _Pid} -> ok;
	{ok, _Pid, _Info} -> ok;
	{error, Error } -> {'EXIT', {start_child_error, Error}}
    end.

stop(_Host) ->
    case supervisor:terminate_child(ejabberd_sup, ejabberd_mod_rest_sup) of
	ok -> ok;
	{error, Error} ->
	    {'EXIT', {terminate_child_error, Error}}
    end.

process([], #request{method = 'POST',
		     data = Data,
		     ip = Ip
		    }) ->
    Stanza = xml_stream:parse_element(Data),
    From = jlib:string_to_jid(xml:get_tag_attr_s("from", Stanza)),
    To = jlib:string_to_jid(xml:get_tag_attr_s("to", Stanza)),
    ?INFO_MSG("Got request from ~s with IP ~p to ~s:~n~p",
	      [jlib:jid_to_string(From),
	       Ip,
	       jlib:jid_to_string(To),
	       Stanza]),
    try
	{xmlelement, "message", _Attrs, _Kids} = Stanza,
	case ejabberd_router:route(From, To, Stanza) of
	    ok -> {200, [], "Ok"};
	    _ -> {500, [], "Error"}
	end
    catch
	error:{badmatch, _} -> {406, [], "Error: can only accept <message/>"};
	  error:{Reason, _} -> {500, [], "Error: " ++ atom_to_list(Reason)}
    end;
process(_Path, _Request) ->
    ?DEBUG("Got request to ~p: ~p", [_Path, _Request]),
    {200, [], "Try POSTing a stanza."}.
