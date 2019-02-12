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
# script is one part of getting that to work. The full steps for setting up the
# environment are documented in the Daffodil Release Workflow wiki page.

# We run a few wine commands, let's disable all wine debug information since it
# is interpreted by sbt as errors and makes the output look like something
# failed.
export WINEDEBUG=-all

# If the WiX environment is setup correctly, the sbt native-packager will
# execute a symlink to this script, and the $0 variable should look something
# like "path/to/wix_directory/\\bin\\candle.exe". The following will extract
# evertyhing after the last backslash, returning either "candle.exe" or
# "light.exe" depending on what the plugin executes.
CMD=${0##*\\}

# The paths passed into this script by the plugin are all absolute Linux style
# paths. For arguments that look like a path (i.e. starts with a /), use
# winepath to convert them to a Windows style path that wine applications can
# understand. Leave all other arguments unmodified.
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

# Tell bash to output the command we are about to execute, helpful for
# debugging when something goes wrong with wine
set -x

# Execute wine with the right WiX command and modified arguments
wine $WIX/$CMD "${NEWARGS[@]}"
