			       mod_rest


This is an ejabberd module that adds an HTTP handler that allows HTTP
clients to literally post arbitrary message stanzas to ejabberd. Those
stanzas then get shoved through ejabberd's router just like any other
stanza.

To use this module, follow the general build instructions, and add the
following to your configuration:
Enable the module:
{modules,
 [
  {mod_rest, []},
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

Since this module does not require HTTP authentication to send
messages, you need to restrict the port using a firewall for example.
Otherwise the feature could be abused by attackers to send spam.

It is important that the value indicated in Content-Length matches
exactly the size of the content.


Example Rest call:
==================
POST /rest HTTP/1.1
Host: localhost
Content-Length: 85

<message to="nolan@localhost" from="localhost/rest">
  <body>World</body>
</message>
