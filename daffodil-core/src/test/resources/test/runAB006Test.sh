#!/bin/bash

INITIAL_WD=`pwd`
TEST_WD=`dirname "$0"`

cd "${TEST_WD}"

DAFFODIL_HOME=../../..
#LIB=:$DAFFODIL_HOME/lib
#CLASSPATH=$DAFFODIL_HOME/bin:$LIB/jdom.jar:$LIB/saxon9.jar:$LIB/saxon9-jdom.jar:$LIB/saxon9-xpath.jar:$LIB/saxon9-s9api.jar:$LIB/xerces.jar:$LIB/joda-time-1.6.jar:$LIB/derby.jar:$LIB/derbytools.jar:$LIB/icu4j-4_4_1_1.jar
CLASSPATH=$DAFFODIL_HOME/lib

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
	${DAFFODIL_HOME}/daffodil.sh -s $schema -i $input -r $root > output.xml 2> error.log
	diff $output output.xml	> errorDiff.log
	if [ $? -eq 0 ];then
	    let PASSED="$PASSED+1"
	    echo "  PASSED"
	else
	    echo -e "\E[47;31m\033[1m  FAILED\033[0m"
            echo "Please see error.log"
	fi
    fi
done < testAB006.txt

echo "RUN: $RUN test(s)"
echo "PASSED: $PASSED test(s)"

cd "${INITIAL_WD}"
