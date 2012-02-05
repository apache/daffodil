#!/bin/bash

DAFFODIL_HOME=`dirname $0`
BIN="$DAFFODIL_HOME/bin"
LIB="$DAFFODIL_HOME/lib"
SCALA_HOME="$DAFFODIL_HOME/lib"
CLASSPATH="$BIN/daffodil.jar:$LIB/jdom.jar:$LIB/saxon9.jar:$LIB/saxon9-jdom.jar:$LIB/saxon9-xpath.jar:$LIB/saxon9-s9api.jar:$LIB/xerces.jar:$LIB/joda-time-1.6.jar:$LIB/derby.jar:$LIB/derbytools.jar:$LIB/icu4j-4_4_1_1.jar:$SCALA_HOME/scala-library.jar"

java -Xmx500m -Xss50M -cp $CLASSPATH daffodil.Main $*
