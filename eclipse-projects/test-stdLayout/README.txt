This test module exists to provide a testing ground for the standard schema 
file system layout, to insure that includes/imports work between test and main
resource directories, across different package names, and across different
schemas spread across different package names.

It contains a nested envelope-payload idiom.

There are two "packages" org1 and org2.

Org2 is the bottom layer. It contains base formats, some types, and a payload
element definition. It uses the urn:payload namespace.

Org1 is an enclosing envelope around the payload. Two actually. 
The Outer envelope contains the Inner envelope which contains the Payload.

The inter-references between the outer Org1 layers and nested Org2 Payload 
layer are the source of complexity here.

In addition, TDML files that contain embedded DFDL schemas can contain include
and import statements as well, and those have paths referring to org1 and
org2 as well as DFDL schema XSD files in the src/test/resources/org1/xsd where
there are DFDL XSD files that exist for testing purposes as well. 

All these should end up on the classpath, and so inter-references that use
the package-style naming should work and not care whether the referenced DFDL XSD
file is in the src/test/resources or src/main/resources directories.

As of this writing, embedded schemas are pulled out into temp directory files, 
and so they cannot make use of self-relative schemaLocation in include/import.
