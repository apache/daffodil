#!/bin/bash

INITIAL_WD=`pwd`
TEST_WD=`dirname "$0"`

cd "${TEST_WD}"

DAFFODIL_HOME=..
CLASSPATH=$DAFFODIL_HOME/bin:$DAFFODIL_HOME/lib/jdom.jar:$DAFFODIL_HOME/lib/saxon9.jar:$DAFFODIL_HOME/lib/saxon9-jdom.jar:$DAFFODIL_HOME/lib/saxon9-xpath.jar:$DAFFODIL_HOME/lib/saxon9-s9api.jar:$DAFFODIL_HOME/lib/xerces.jar:$DAFFODIL_HOME/lib/joda-time-1.6.jar:$DAFFODIL_HOME/lib/derby.jar:$DAFFODIL_HOME/lib/derbytools.jar


RUN=0
PASSED=0

while read line 
do
    if [ ${#line} -gt 0 -a ${line:0:1} != "#" ];then
	set -- "$line" 
	IFS=","; declare -a arr=($*) 
	schema=${arr[0]}
	root=${arr[1]}
	input=${arr[2]}
	output=${arr[3]}
	
	echo "TESTING $schema on input $input..."
	let RUN="$RUN+1"
	scala -cp $CLASSPATH daffodil.Main -s $schema -D tempParser.bin
	scala -cp $CLASSPATH daffodil.Main -p tempParser.bin -i $input -r $root > output.xml
	diff $output output.xml	
	if [ $? -eq 0 ];then
	    let PASSED="$PASSED+1"
	    echo "  PASSED"
	else
	    echo "  FAILED"
	fi
    fi
done < testSuite.txt

echo "RUN: $RUN tests"
echo "PASSED: $PASSED tests"

rm tempParser.bin

cd "${INITIAL_WD}"
