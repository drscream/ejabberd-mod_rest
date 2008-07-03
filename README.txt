			       mod_rest

This is an ejabberd module that adds an HTTP handler that allows HTTP
clients to literally post arbitrary message stanzas to ejabberd. Those
stanzas then get shoved through ejabberd's router just like any other
stanza.

To use this module, follow the general build instructions, and add the
following to your configuration, among the other modules:

{mod_rest, []}


Example Rest call:
==================
POST /rest HTTP/1.1
Host: localhost
Content-Length: 85

<message to="nolan@localhost" from="localhost/rest">
  <body>World</body>
</message>
