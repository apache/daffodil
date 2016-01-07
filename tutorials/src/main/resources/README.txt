About the tutorials-from-tdml.

The XSD for tdml has been modified to allow tdml:tutorial elements containing XHTML5 markup.

This allows a tdml file to generate a nice XHTML5 web page describing the content using an XSLT stylesheet.

A CSS stylesheet provides the visual characteristics.

The file remains a runnable TDML test file - thereby allowing you to keep the tests functional and
in synch with the tutorial content. 

NOTE: using Firefox on 2016-01-06 you can just use a file:// URL to open the tdml file. It then accesses the xsl
and css from the same directory. 

Chrome does not work the same. It refuses to allow opening files on the local file system for security reasons.
Chrome also doesn't look inside the tdml file to guess that it is XML content. It can only guess based on the extension, and
it knows nothing of .tdml extension.

To win with Chrome you must serve the page from a web server.

Fortunately, there's a near trivial way to do this. 

Below is a shell script that runs (on linux anyway), and starts a simple  web server that is
built in to python implementations. 

If you run this shell script in this directory (where bitorder.tut.tdml and the related xsl and css files are),
then when the page is served, Chrome will both recognize that .tdml is XML, and also will allow the
tdml file to trigger the XSLT transform using the stylesheet.

You would browse the example tutorial here via http://localhost:8000/bitorder.tut.tdml

(Once you have this server started, you can also use this same URL from Firefox.)

#!/usr/bin/python

import SimpleHTTPServer
import SocketServer

PORT = 8000

Handler = SimpleHTTPServer.SimpleHTTPRequestHandler
Handler.extensions_map.update({
    '.tdml': 'application/xml',
});

httpd = SocketServer.TCPServer(("", PORT), Handler)

print "Serving at port", PORT
httpd.serve_forever()


