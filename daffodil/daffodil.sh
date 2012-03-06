#!/bin/bash

DAFFODIL_HOME=`dirname $0`
LIB="$DAFFODIL_HOME/lib"
CLASSPATH=$LIB/*

java -Xmx500m -Xss50M -cp "$CLASSPATH" daffodil.Main "$@"
