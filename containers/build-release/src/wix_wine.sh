#!/bin/bash
#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# We use sbt-native-packager plugin to build a Windows MSI installer.
# Unfortunately, that uses WiX toolset which requires Windows. It can be run
# under Wine, but the plugin cannot be easily configured to use Wine. This
# script is one part of getting that to work and is setup within the container
# environment as needed.


# We run a few wine commands, let's disable all wine debug information since it
# is interpreted by sbt as errors and makes the output look like something
# failed.
export WINEDEBUG=-all

# Create initial wine config, redirecting stderr to stdout. The command outputs
# debug message to stderr, which SBT makes look like an actual error.
winecfg 2>&1

# The sbt native-packager plugin executes the $WIX/{candle,light}.exe
# executables to build the Daffodil MSI. The problem is that those are Windows
# executables and so can't be directly executed in the Linux container. To get
# around this, the container Dockerfile copies the $WIX/{candle,light}.exe
# files to $WIX/real-{candle,light}.exe and installs this script to
# $WIX/{candle,light}.exe. This way, when the sbt plugin executes
# $WIX/{candle,light}.exe, it's actually executing this script, which figures
# out how it was executed (either as candle.exe or light.exe) and redirects the
# execution to the real-{candle,light}.exe file using wine.


# Figure out what was originally executed, and prepend "real-" to it. This is
# what to execute with wine
REAL_CMD=real-$(basename "$0")

# The paths passed as arguments to this script by the plugin are all absolute
# Linux style paths. For arguments that look like a path (i.e. starts with a
# forward slash), use winepath to convert them to a Windows style path that
# wine applications can understand. Leave all other arguments unmodified.
i=0
NEWARGS=()
for VAR in "$@"
do
	if [[ $VAR == /* ]]
	then
		NEWARGS[$i]=$(winepath -w "$VAR")
	else
		NEWARGS[$i]=$VAR
	fi
	((++i))
done

# Uncomment to tell bash to output the wine command we are about to execute,
# helpful for debugging when something goes wrong with wine
# set -x

# Execute wine with the real WiX command and modified arguments
wine $WIX/$REAL_CMD "${NEWARGS[@]}"
