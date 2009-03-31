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
		     host = Host,
		     ip = ClientIp
		    }) ->
    maybe_post_request(Data, Host, ClientIp);
process(_Path, _Request) ->
    ?DEBUG("Got request to ~p: ~p", [_Path, _Request]),
    {200, [], "Try POSTing a stanza."}.


maybe_post_request(Data, Host, ClientIp) ->
    try
	Stanza = xml_stream:parse_element(Data),
        From = jlib:string_to_jid(xml:get_tag_attr_s("from", Stanza)),
        To = jlib:string_to_jid(xml:get_tag_attr_s("to", Stanza)),
	allowed = check_stanza(Stanza, From, To, Host, ClientIp),
	?INFO_MSG("Got valid request from ~s~nwith IP ~p~nto ~s:~n~p",
		  [jlib:jid_to_string(From),
		   ClientIp,
		   jlib:jid_to_string(To),
		   Stanza]),
	post_request(Stanza, From, To)
    catch
	error:{badmatch, _} ->
	    {406, [], "Error: REST request is rejected by service."};
	error:{Reason, _} ->
	    {500, [], "Error: " ++ atom_to_list(Reason)};
	_ ->
	    {500, [], "Error"}
    end.

%% This function crashes if the stanza does not satisfy configured restrictions
check_stanza(Stanza, _From, To, Host, {ClientAddress, _PortNumber}) ->
    check_member_option(Host, ClientAddress, allowed_ips),
    check_member_option(Host, jlib:jid_to_string(To), allowed_destinations),
    {xmlelement, StanzaType, _Attrs, _Kids} = Stanza,
    check_member_option(Host, StanzaType, allowed_stanza_types),
    allowed.

check_member_option(Host, Element, Option) ->
    true = case gen_mod:get_module_opt(Host, ?MODULE, Option, all) of
	       all -> true;
	       AllowedValues -> lists:member(Element, AllowedValues)
	   end.

post_request(Stanza, From, To) ->
    case ejabberd_router:route(From, To, Stanza) of
	ok -> {200, [], "Ok"};
        _ -> {500, [], "Error"}
    end.
