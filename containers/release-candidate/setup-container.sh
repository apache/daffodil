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

# sbt rpm is self hosted
cat <<EOF > /etc/yum.repos.d/sbt-rpm.repo
[sbt-rpm]
name=sbt-rpm
baseurl=https://repo.scala-sbt.org/scalasbt/rpm
gpgcheck=0
repo_gpgcheck=0
enabled=1
EOF

# install dependencies
microdnf -y install \
  clang \
  git \
  java-1.8.0-devel \
  llvm \
  mxml-devel \
  pinentry \
  rpm-build \
  rpm-sign \
  sbt \
  subversion \
  unzip \
  vim-minimal \
  wine \
  winetricks

# install wix
curl -L https://github.com/wixtoolset/wix3/releases/download/wix3112rtm/wix311-binaries.zip -o wix311-binaries.zip
mkdir wix311
unzip wix311-binaries.zip -d wix311/
rm wix311-binaries.zip

# enable sbt pgp
mkdir -p /root/.sbt/1.0/plugins/
sh -c "echo 'addSbtPlugin(\"com.jsuereth\" % \"sbt-pgp\" % \"2.0.1\")' >> /root/.sbt/1.0/plugins/pgp.sbt"
