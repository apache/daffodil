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

# To make for reproducibility, this should match the version used by GitHub
# actions CI release candidate workflow
FROM docker.io/library/ubuntu:22.04
WORKDIR /root

# Delete any cruft in the homedir
RUN rm -rf /root/*

ENV TZ=UTC

# Install/configure dependencies. Note that these are not pinned any any
# specific version, so depending on when this Docker image is built, it could
# result in different in changes to the build. Note that the sbt and yarn
# versions don't really matter all too much since they are just used to
# bootstrap--the daffodil projects specify the actual versions of yarn/sbt to
# use/download when building. We also need to install SBT separtely since we
# need to install dependencies like curl and gpg to verify SBT signature
RUN \
  dpkg --add-architecture i386 && \
  apt-get update && \
  apt-get install -qy \
    clang \
    curl \
    git \
    gpg \
    libmxml-dev \
    llvm \
    npm \
    openjdk-8-jdk-headless \
    rpm \
    unzip \
    wine32 \
    winetricks && \
  echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" > /etc/apt/sources.list.d/sbt.list && \
  curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | \
    gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import && \
  chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg && \
  apt-get update && \
  apt-get install -y \
    sbt && \
  npm --no-update-notifier install --global yarn@1.22.19 node@16 && \
  winetricks -q dotnet40

# WIX is a Microsoft Windows Installer creation tool kit.
#
# Install wix, including changes to allow WiX to run in wine on Linux. See
# src/wix_wine.sh for more details on why we need to do this and how it works
#
# Updating WIX should be done only if there is a specific need (for security, or other compelling reason)
# because it is likely things will break and the release scripts/process will have to adapt.
# The WIX version 3.11.2 is hard coded into these script lines as tokens wix3112rtm and wix311.
#
# In order to ensure we are downloading and using the exact WIX binary we have tested and trust
# we verify the sha512 is the same as the one expected. This protects us from if someone
# was to get github credentials allowing them to change the wix binaries on github.
# If WIX is updated to a newer version, this sha512 will need to be recomputed.
RUN \
  WIXSHA=6fd961c85e1e6adafb99ef50c9659e3eb83c84ecaf49f523e556788845b206e1857aba2c39409405d4cda1df9b30a552ce5aab808be5f8368a37a447d78d1a05 && \
  curl -sS -L https://github.com/wixtoolset/wix3/releases/download/wix3112rtm/wix311-binaries.zip -o wix311-binaries.zip && \
  echo "$WIXSHA wix311-binaries.zip" | sha512sum --quiet -c - && \
  mkdir /opt/wix && \
  unzip -q wix311-binaries.zip -d /opt/wix/ && \
  rm wix311-binaries.zip
RUN mv /opt/wix/candle.exe /opt/wix/real-candle.exe
RUN mv /opt/wix/light.exe /opt/wix/real-light.exe
COPY src/wix_wine.sh /opt/wix/candle.exe
COPY src/wix_wine.sh /opt/wix/light.exe

# We expect users to mount the project repo to /root/project, so we change that
# to our working directory so the daffodil-build-release command is run from
# the repo directory
WORKDIR "/root/project"

# Install and set the entrypoint
COPY src/daffodil-build-release /usr/bin/
ENTRYPOINT ["/usr/bin/daffodil-build-release"]
