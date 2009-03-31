
	mod_rest - HTTP interface to POST stanzas into ejabberd

	Author: Nolan Eakins <sneakin@semanticgap.com>
	Copyright (C) 2008 Nolan Eakins

	Requirements: ejabberd trunk SVN



This is an ejabberd module that adds an HTTP handler that allows HTTP
clients to literally post arbitrary message stanzas to ejabberd. Those
stanzas then get shoved through ejabberd's router just like any other
stanza.


	CONFIGURATION
	=============

To use this module, follow the general build instructions, and add the
following to your configuration:
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
  allowed_ips: Define which IP addresses can connect to the rest service.
  Allowed values are: 'all', or a list of allowed IP addresses, expressed as Erlang tuples.
  Default value: all
  In this example only the two defined IP addresses are allowed:
    {modules,
     [
      {mod_rest, [{allowed_ips, [ {127,0,0,1}, {192, 168, 1, 12} ]}]},
      ...
     ]
    }.

Since this module does not require HTTP authentication to send
messages, you need to restrict the port using a firewall or allowed_ips option.
Otherwise the feature could be abused by attackers to send spam.

In ejabberd 2.0.x versions,
it is important that the value indicated in Content-Length matches
exactly the size of the content.


	EXAMPLE REST CALL
	=================

POST /rest HTTP/1.1
Host: localhost
Content-Length: 85

<message to="nolan@localhost" from="localhost/rest"><body>World</body></message>



	EXAMPLE CALL FROM SHELL
	=======================

Here are two simple ways to send this POST, using Lynx or Wget:

$ lynx http://localhost:5280/rest/ -mime_header -post_data
<message to="nolan@localhost" from="localhost/rest"><body>World</body></message>
---
HTTP/1.0 200 OK
Connection: close
Content-Type: text/html; charset=utf-8
Content-Length: 2

Ok

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

ejabberd.log shows those messages:
=INFO REPORT==== 2-Mar-2009::11:46:05 ===
I(<0.484.0>:ejabberd_listener:201) : (#Port<0.3661>) Accepted connection {{127,0,0,1},55945} -> {{127,0,0,1},5280}

=INFO REPORT==== 2-Mar-2009::11:46:05 ===
I(<0.251.0>:ejabberd_http:127) : started: {gen_tcp,#Port<0.3661>}

=INFO REPORT==== 2-Mar-2009::11:46:05 ===
I(<0.515.0>:mod_rest:71) : Got request from localhost/rest with IP
{{127,0,0,1}, 55945} to nolan@localhost:
{xmlelement,"message",
            [{"to","nolan@localhost"},{"from","localhost/rest"}],
            [{xmlelement,"body",[],[{xmlcdata,<<"World">>}]}]}


If the user nolan@localhost exists, it will receive this message:
<message from='localhost/rest'
	 to='nolan@localhost'>
  <body>World</body>
</message>

