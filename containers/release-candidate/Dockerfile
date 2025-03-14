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

FROM registry.fedoraproject.org/fedora:41

WORKDIR /root

# Delete cruft fedora leaves in the homedir
RUN rm -rf /root/*

# Install dependencies, explicitly not enabling the updates repo so we are
# pinned at a particular fedora release.
RUN \
  dnf -y --quiet --repo=fedora install \
    clang \
    git \
    java-1.8.0-devel \
    llvm \
    mxml-devel \
    npm \
    pandoc \
    pinentry \
    rpm-build \
    rpm-sign \
    subversion \
    unzip \
    vim-minimal \
    wine \
    winetricks \
    xorg-x11-server-Xvfb && \
  dnf clean all

# Enable sbt-pgp plugin
COPY src/plugins.sbt /root/.sbt/1.0/plugins/

# Download and install Inno Setup via wine. Even though we provide options to
# make Inno Setup a headless install, wine still thinks it wants to open a
# window so we need install with a temporary virtual X server with xvfb-run.
# Also ensure it is installed in the directory where the deafult Daffodil SBT
# configuration expects it--different wine configurations could install it
# somewhere else.
RUN \
  INNO_SETUP_EXE="innosetup-6.4.1.exe" && \
  EXPECTED_SHA=5dfc9999e2feafa28754baaf80cf73ac96414228b94a1132a919554a822c892810197305d9355885b9ac408c214691cd45279fc2df3a891fbebc4f8eb86bac87 && \
  curl -sS -L https://files.jrsoftware.org/is/6/$INNO_SETUP_EXE -o $INNO_SETUP_EXE && \
  echo "$EXPECTED_SHA $INNO_SETUP_EXE" | sha512sum --quiet -c - && \
  winetricks -q win10 && \
  xvfb-run wine $INNO_SETUP_EXE /VERYSILENT /SUPPRESSMSGBOXES /TASKS= '/DIR=C:\\Program Files (x86)\\Inno Setup 6' && \
  rm $INNO_SETUP_EXE

# Install a pinned version of sbt. This is used only as a bootstrap so does not need to be updated.
# This version of sbt is not used to build any Daffodil project parts, as they specify their own sbt versions.
RUN \
  dnf -y --quiet --repofrompath=sbt-rpm,https://repo.scala-sbt.org/scalasbt/rpm --repo=sbt-rpm --nogpgcheck install \
    sbt-1.7.1 && \
  dnf clean all

# Install a pinned version of yarn. This is used only as a bootstrap so does not need to be updated.
# This version of yarn is not used to build any Daffodil project parts, as they specify their own yarn versions.
RUN \
  npm --no-update-notifier install --global yarn@1.22.19

# Install and set the entrypoint
COPY src/daffodil-release-candidate /usr/bin/
ENTRYPOINT ["/usr/bin/daffodil-release-candidate"]
