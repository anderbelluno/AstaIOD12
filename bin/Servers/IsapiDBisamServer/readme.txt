AstaIO for use in an isapi dll
==============================

This examples contains 2 pieces a server side and client side.

Server Side
===========
AstaIOHttpServer.dpr is an example of an isapi dll that contains a full
AstaIO Server. Inside the Isapi dll are an AstaIOSTringClientwire and AstaIOStringServer
wire that basically creates a 2 tier application just like client server.

The ClientWire and ServerWire share a String buffer so that communication is
very fast.

AstaIOhttpServer.dll implements a full dbisam AstaIO server.

the dbisam directory is defined in the StringDM.pas GetDatabase Directory.

the server can handle client side sql or providers  or servermethods.


Client side
============
Uses a TastaIOwebClientWire with these settings:

1.Username/password for authentication. current server authenticates anybody
2. use winInet set to true so that IE settings will get through any firewall
3. Clientwire.WebServer settings:

   a. Address of web server
   b. Port
   c. scripts (where the dll is) isapi/AstaHttpServer.dll in this case
   d. timeout
   e. UseWebServer set to true.

Additoinally you can set compression and encryption options if we want to
run this securely.

The client demo shows how to do a select and then call applyupdats.
by default AstaIOclientQueries have their updateMethods set to cached.
this allows you to insert,edit,delete and then just call applyUpdates.
 


