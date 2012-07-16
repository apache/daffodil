#!/bin/bash

DAFFODIL_HOME=..
LIB=$DAFFODIL_HOME/lib
SCALA_HOME=/usr/share/java
CLASSPATH=$DAFFODIL_HOME/bin:$LIB/jdom.jar:$LIB/saxon9.jar:$LIB/saxon9-jdom.jar:$LIB/saxon9-xpath.jar:$LIB/saxon9-s9api.jar:$LIB/xerces.jar:$LIB/joda-time-1.6.jar:$LIB/scala-library.jar:$LIB/icu4j-4_4_1_1.jar


RUN=0
PASSED=0
START=`date +%s`
while read line 
do
    if [ ${line:0:1} != "#" ];then
	set -- "$line" 
	IFS=","; declare -a arr=($*) 
	schema=${arr[0]}
	root=${arr[1]}
	input=${arr[2]}
	output=${arr[3]}
	
	echo "TESTING $schema on input $input..."
	let RUN="$RUN+1"
	java -Xmx2g -Xss100M -cp $CLASSPATH daffodil.Main -s $schema -i $input -r $root > output.xml 2> error.log
	diff $output output.xml	> error.log
	if [ $? -eq 0 ];then
	    let PASSED="$PASSED+1"
	    echo "  PASSED"
	else
	    echo "  FAILED"
	fi
    fi
done < testSuiteStress.txt
END=`date +%s`
let TOTAL="$END-$START"
echo "RUN: $RUN tests"
echo "PASSED: $PASSED tests"
echo "TIME: $TOTAL seconds"
