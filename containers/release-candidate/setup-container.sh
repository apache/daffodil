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

export BUILD_DIR=`pwd`
export WINEDEBUG=-all

#delete stuff in homedir
rm ~/*

#install dependencies
curl https://bintray.com/sbt/rpm/rpm -o /etc/yum.repos.d/bintray-sbt-rpm.repo
microdnf install git svn sbt java-1.8.0-devel wine winetricks unzip rpm-build rpm-sign vim-minimal

# install wix
curl -L https://github.com/wixtoolset/wix3/releases/download/wix3111rtm/wix311-binaries.zip -o wix311-binaries.zip
mkdir wix311
unzip wix311-binaries.zip -d wix311/
rm wix311-binaries.zip

# enable sbt pgp
mkdir -p /root/.sbt/1.0/plugins/
sh -c "echo 'addSbtPlugin(\"com.jsuereth\" % \"sbt-pgp\" % \"2.0.1\")' >> /root/.sbt/1.0/plugins/pgp.sbt"

# pre-download sbt version used by daffodil
TMP_SBT_PROJECT=/tmp/sbt-project/
mkdir -p $TMP_SBT_PROJECT
pushd $TMP_SBT_PROJECT &> /dev/null
sbt --sbt-version 1.5.0 exit
popd &> /dev/null
rm -rf $TMP_SBT_PROJECT
