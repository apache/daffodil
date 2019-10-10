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

set -e

command -v rpmbuild &> /dev/null || { echo -e "\n!!! rpmbuild command required !!!\n"; exit; }
command -v wine &> /dev/null || { echo -e "\n!!! wine command required, please set up the environment for WiX !!!\n"; exit; }
command -v $WIX/\\bin\\candle.exe &> /dev/null || { echo -e "\n!!! WiX environment not set up correctly, missing candle.exe !!!\n"; exit; }
command -v $WIX/\\bin\\light.exe &> /dev/null || { echo -e "\n!!! WiX environment not set up correctly, missing light.exe !!!\n"; exit; }

if [ -n "$(git status --porcelain)" ]; then { echo -e "\n!!! Repository is not clean. Commit changes or clean repo before building release !!!\n"; git status; exit; } fi

VERSION=$(grep 'version :=' build.sbt | cut -d\" -f2)
if [[ $VERSION == *SNAPSHOT* ]]; then echo -e "\n!!! SBT Version ($VERSION) should not contain SNAPSHOT for a release !!!\n"; exit; fi

read -p "Pre Release label (e.g. rc1): " PRE_RELEASE
if [ -z "$PRE_RELEASE" ]; then echo -e "\n!!! Pre Release label must not be empty !!!\n"; exit; fi

read -e -p "Path to Apache dist directory: " APACHE_DIST
DAFFODIL_DIST_ROOT="$APACHE_DIST/dev/incubator/daffodil/"
DAFFODIL_RELEASE_DIR=$DAFFODIL_DIST_ROOT/$VERSION-$PRE_RELEASE
if [ ! -d "$DAFFODIL_DIST_ROOT" ]; then echo -e "\n!!! Daffodil release root directory does not exist: $DAFFODIL_DIST_ROOT !!!\n"; exit; fi
if [ -d "$DAFFODIL_RELEASE_DIR" ]; then
    read -p "Output directory already exists: '$DAFFODIL_RELEASE_DIR'. Remove it? (Y/n) " REMOVE
    if [[ -z "$REMOVE" || "$REMOVE" == "Y" || "$REMOVE" == "y" ]]; then
        rm -rf "$DAFFODIL_RELEASE_DIR"
    else
        echo "Exiting."
        exit
    fi
fi    

read -e -p "Path to Apache Daffodil site directory: " DAFFODIL_SITE_DIR
DAFFODIL_DOCS_DIR=$DAFFODIL_SITE_DIR/site/docs/$VERSION/
if [ ! -d "$DAFFODIL_SITE_DIR" ]; then echo -e "\n!!! Daffodil site directory does not exist: $DAFFODIL_SITE_DIR !!!\n";  exit; fi
if [ -d "$DAFFODIL_DOCS_DIR" ]; then
    read -p "Daffodil site doc version directory already exists: '$DAFFODIL_DOCS_DIR'. Remove it? (Y/n) " REMOVE
    if [[ -z "$REMOVE" || "$REMOVE" == "Y" || "$REMOVE" == "y" ]]; then
        rm -rf "$DAFFODIL_DOCS_DIR"
    else
        echo "Exiting."
        exit
    fi
fi

DAFFODIL_TUTORIALS_DIR=$DAFFODIL_SITE_DIR/tutorials/
if [ -d "$DAFFODIL_TUTORIALS_DIR" ]; then 
   read -p "Daffodil site tutorials directory already exists: '$DAFFODIL_TUTORIAL_DIR'. Remove it? (Y/n) " REMOVE
   if [[ -z "$REMOVE" || "$REMOVE" == "Y" || "$REMOVE" == "y" ]]; then
       rm -rf "$DAFFODIL_TUTORIALS_DIR"
   else
       echo "Exiting."
       exit
   fi
fi

read -p "Signing Key ID (long format): " PGP_SIGNING_KEY_ID
read -p "Apache Username: " APACHE_USERNAME
read -s -p "Apache Password: " APACHE_PASSWD


echo
echo
echo "!!! Making release $VERSION-$PRE_RELEASE in $DAFFODIL_RELEASE_DIR !!!"
echo

echo "Create directories:  mkdir -p $DAFFODIL_RELEASE_DIR/{src,bin}/"
mkdir -p $DAFFODIL_RELEASE_DIR/{src,bin}/
echo "Create directories:  mkdir -p $DAFFODIL_DOCS_DIR/{javadoc,scaladoc}/"
mkdir -p $DAFFODIL_DOCS_DIR/{javadoc,scaladoc}/

echo "Creating Source Tarball..."
git archive --format=zip --prefix=apache-daffodil-$VERSION-incubating-src/ HEAD > $DAFFODIL_RELEASE_DIR/src/apache-daffodil-$VERSION-incubating-src.zip

echo "Building Convenience Binaries and Publishing to Apache Repository..."
sbt \
  "set updateOptions in ThisBuild := updateOptions.value.withGigahorse(false)" \
  "set credentials in ThisBuild += Credentials(\"Sonatype Nexus Repository Manager\", \"repository.apache.org\", \"$APACHE_USERNAME\", \"$APACHE_PASSWD\")" \
  "set publishTo in ThisBuild := Some(\"Apache Staging Distribution Repository\" at \"https://repository.apache.org/service/local/staging/deploy/maven2\")" \
  "set pgpSigningKey := Some(new java.math.BigInteger(\"$PGP_SIGNING_KEY_ID\", 16).longValue)" \
  "+compile" \
  "+publishSigned" \
  "daffodil-cli/rpm:packageBin" \
  "daffodil-cli/windows:packageBin" \
  "daffodil-cli/universal:packageBin" \
  "daffodil-cli/universal:packageZipTarball" \
  "daffodil-japi/genjavadoc:doc" \
  "daffodil-sapi/doc" \

cp daffodil-cli/target/universal/apache-daffodil-*.tgz $DAFFODIL_RELEASE_DIR/bin/
cp daffodil-cli/target/universal/apache-daffodil-*.zip $DAFFODIL_RELEASE_DIR/bin/
cp daffodil-cli/target/rpm/RPMS/noarch/apache-daffodil-*.rpm $DAFFODIL_RELEASE_DIR/bin/
MSI_NAME=$(basename $DAFFODIL_RELEASE_DIR/bin/*.zip .zip).msi
cp daffodil-cli/target/windows/Daffodil.msi $DAFFODIL_RELEASE_DIR/bin/$MSI_NAME
chmod -x $DAFFODIL_RELEASE_DIR/bin/$MSI_NAME

cp -R daffodil-japi/target/scala-2.12/genjavadoc-api/* $DAFFODIL_DOCS_DIR/javadoc/
cp -R daffodil-sapi/target/scala-2.12/api/* $DAFFODIL_DOCS_DIR/scaladoc/

cp -R tutorials/src/main/resources/* $DAFFODIL_TUTORIALS_DIR

echo "Calculating Checksums..."
for i in src/ bin/
do
    pushd $DAFFODIL_RELEASE_DIR/$i > /dev/null
    for file in *
    do
       sha256sum --binary $file > $file.sha256
       sha512sum --binary $file > $file.sha512
       gpg --default-key $PGP_SIGNING_KEY_ID --detach-sign --armor --output $file.asc $file
    done
    popd > /dev/null
done

echo "Creating Git tag..."
git tag -as -u $PGP_SIGNING_KEY_ID -m "Release v$VERSION-$PRE_RELEASE"  v$VERSION-$PRE_RELEASE

echo ""
echo ""
echo "!!! Finished: $VERSION-$PRE_RELEASE output to $DAFFODIL_RELEASE_DIR !!!"

echo "Things to verify: "
echo
echo "- Files in $DAFFODIL_RELEASE_DIR"
echo "- Files in $DAFFODIL_DOCS_DIR"
echo "- Staged published files at https://repository.apache.org/"
echo "- Git tag created at v$VERSION-$PRE_RELEASE"
echo
echo "If that all looks corect, 'svn ci' the new files in $DAFFODIL_RELEASE_DIR, close the published Nexus files, 'git push origin v$VERSION-$PRE_RELEASE', and start a VOTE"
echo
