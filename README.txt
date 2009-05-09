
	mod_rest - HTTP interface to POST stanzas into ejabberd

	Author: Nolan Eakins <sneakin@semanticgap.com>
	Copyright (C) 2008 Nolan Eakins

	Requirements: ejabberd trunk SVN 2025 (ejabberd 2.1.0, once released)



This is an ejabberd module that adds an HTTP handler that allows HTTP
clients to literally post arbitrary message stanzas to ejabberd. Those
stanzas then get shoved through ejabberd's router just like any other
stanza.

This module can also be used as a frontend to execute ejabberd commands.


	CONFIGURATION
	=============

To use this module, follow the general build instructions, and configure
in ejabberd.cfg as described.

Enable the module:
{modules,
 [
  {mod_rest, [ {allowed_ips, [ {127,0,0,1} ]} ]},
  ...
 ]
}.

And enable the HTTP request handler in the listen section:
{listen,
 [
  ...
  {5285, ejabberd_http, [
                         {request_handlers, [
                                             {["rest"], mod_rest}
                                            ]}
                        ]}
 ]
}.

With that configuration, you can send HTTP POST requests to the URL:
  http://localhost:5285/rest

Configurable options:

  allowed_ips: IP addresses that can use the rest service.
  Allowed values: 'all' or a list of Erlang tuples.
  Default value: all
  Notice that the IP address is checked after the connection is established.
  If you want to restrict the IP address that listens connections, and
  only allow a certain IP to be able to connect to the port, then the
  option allowed_ips is not useful to you: you better define the
  listening IP address in the ejabberd listeners (see the ejabberd Guide).

  allowed_destinations: Allowed destination Jabber ID addresses in the stanza.
  Allowed values: 'all' or a list of strings.
  Default value: all

  allowed_stanza_types: Allowed stanza types of the posted stanza.
  Allowed values: 'all' or a list of strings.
  Default value: all

  access_commands: Access restrictions to execute ejabberd commands.
  This option is similar to the option ejabberdctl_access_commands that 
  is documented in the ejabberd Guide.
  There is more information about AccessCommands in the ejabberd Guide.
  Default value: []

Complex example configuration:
{modules,
 [
  {mod_rest, [
              {allowed_ips, [ {127,0,0,1}, {192,168,1,12} ]},
              {allowed_destinations, [ "nolan@localhost", "admin@example.com" ]},
              {allowed_stanza_types, [ "message", "presence", "iq" ]},
              {access_commands, [ {configure, [registered_users], []} ]}
             ]
  },
  ...
 ]
}.

This module gives many power to perform tasks in ejabberd,
such power in bad hands can harm your server, so you should  
restrict the IP address that can connect to the service using:
a firewall, allowed_ips option, or the listener IP option.

In ejabberd 2.0.x versions,
it is important that the value indicated in Content-Length matches
exactly the size of the content.


	EXAMPLE REST CALL
	=================

When the module receives this:
-------
POST /rest HTTP/1.1
Host: localhost
Content-Length: 85

<message to="nolan@localhost" from="localhost/rest"><body>World</body></message>
-------

ejabberd.log shows those messages:
-------
=INFO REPORT==== 2-Mar-2009::11:46:05 ===
I(<0.484.0>:ejabberd_listener:201) : (#Port<0.3661>) Accepted connection {{127,0,0,1},55945} -> {{127,0,0,1},5280}

=INFO REPORT==== 2-Mar-2009::11:46:05 ===
I(<0.251.0>:ejabberd_http:127) : started: {gen_tcp,#Port<0.3661>}

=INFO REPORT==== 2-Mar-2009::11:46:05 ===
I(<0.841.0>:mod_rest:81) : Got request from localhost/rest
with IP {{127,0,0,1},49613}
to nolan@localhost:
{xmlelement,"message",
            [{"to","nolan@localhost"},{"from","localhost/rest"}],
            [{xmlelement,"body",[],[{xmlcdata,<<"World">>}]}]}
-------

If the user nolan@localhost exists, he will receive this message:
-------
<message from='localhost/rest'
	 to='nolan@localhost'>
  <body>World</body>
</message>
-------

Instead of an XMPP stanza, you can provide an ejabberd command to execute:
registered_users localhost

If you configure access_commands in mod_rest, you need to provide information
about a local Jabber account with enough privileges according to your option:
--auth robot localhost pass0011 registered_users localhost


	EXAMPLE CALL WITH LYNX
	======================

This example shows how to send a POST using Lynx:

$ lynx http://localhost:5280/rest/ -mime_header -post_data
<message to="nolan@localhost" from="localhost/rest"><body>World</body></message>
---
HTTP/1.0 200 OK
Connection: close
Content-Type: text/html; charset=utf-8
Content-Length: 2

Ok


	EXAMPLE CALL WITH WGET
	======================

This example shows how to send a POST using Wget:

$ wget http://localhost:5280/rest/ --server-response --post-data '<message to="nolan@localhost" from="localhost/rest"><body>World</body></message>'

--2009-03-02 12:01:42--  http://localhost:5280/rest/
Resolving localhost... 127.0.0.1
Connecting to localhost|127.0.0.1|:5280... connected.
HTTP request sent, awaiting response...
  HTTP/1.0 200 OK
  Connection: keep-alive
  Content-Type: text/html; charset=utf-8
  Content-Length: 2
Length: 2 [text/html]
Saving to: `index.html'

100%[======================================>] 2           --.-K/s   in 0s

2009-03-02 12:01:42 (285 KB/s) - `index.html' saved [2/2]


The content of the index.html is simply:
Ok


	EXAMPLE CALL WITH PYTHON
	========================

This example Python code first calls to send a stanza, and then calls to execute a command:
-------
import urllib2

server_url = 'http://localhost:5280/rest/'

call = '<message to="user1@localhost" from="localhost/rest"><body>World</body></message>'
resp = urllib2.urlopen(server_url, call)
result = resp.read()
print result

call = 'registered_users localhost'
resp = urllib2.urlopen(server_url, call)
result = resp.read()
print result
-------
