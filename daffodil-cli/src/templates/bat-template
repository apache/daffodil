@echo off
REM DAFFODIL_CLASSPATH
REM
REM The Daffodil implementation will look on the classpath for 'includes' and
REM 'imports'. To defined additional directories where Daffodil should look for
REM files, set the DAFFODIL_CLASSPATH environment variable, for example:
REM
REM   set DAFFODIL_CLASSPATH="/path/to/imports/;/path/to/includes/"
REM
REM In addition to defining directories to search for imports and includes, you
REM can add a "CatalogManager.properties" file to the DAFFODIL_CLASSPATH to
REM direct Daffodil to a relative path location of a user XML Catalog.
REM
REM
REM DAFFODIL_JAVA_OPTS
REM
REM If you need to specify java options specific to Daffodil, you can set the
REM DAFFODIL_JAVA_OPTS environment variable. If not specified, the JAVA_OPTS
REM environment variable will be used. If that is not specified, reasonable
REM defaults for Daffodil will be defined.


set MAINCLASS=edu.illinois.ncsa.daffodil.Main
set BINDIR=%~dp0
set LIBDIR=%BINDIR%..\lib
set CLASSPATH=%LIBDIR%\*

set JOPTS=-Xms1024m -Xmx1024m -XX:ReservedCodeCacheSize=128m

if defined DAFFODIL_JAVA_OPTS (
	set JOPTS=%DAFFODIL_JAVA_OPTS%
) else if defined JAVA_OPTS (
	set JOPTS=%JAVA_OPTS%
)

java %JOPTS% -cp "%CLASSPATH%;%DAFFODIL_CLASSPATH%" %MAINCLASS% %*
