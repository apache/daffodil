This directory contains source and documentation jars/zips or folders for the library content in the lib directory.

This is isolated separately because it isn't needed as part of any distribution. It is here so that the source and javadoc for any libraries is available and at ones fingertips when using an IDE for development. Eclipse knows where all these sources and javadocs are, and makes them instantly available. 

Everything is version labeled. When a library is distributed with jars/zips that do not explicitly contain the version number, then a subdirectory containing the version number is created.

Unfortunately, some things have to be decompressed. They don't seem to work as zips as originally constructed due perhaps to folder structure, I'm not sure. Rather than leave them fully unzipped. They've been rezipped with shallower folder structure.

Note about Saxon-B. Version 9.1.0.1 is the last to have javadoc distributed. Newer jars/downloads don't have it in them, so that is why the "saxon-resources" zip and the jar in the lib directory have different versions.