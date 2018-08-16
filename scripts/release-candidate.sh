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

if [ -n "$(git status --porcelain)" ]
then
	echo
	echo "!!! Repository is not clean. Commit changes or clean repo before building release !!!"
	echo
	git status
	exit
fi

VERSION=$(grep 'version :=' build.sbt | cut -d\" -f2)
if [[ $VERSION == *SNAPSHOT* ]]
then
	echo
	echo "!!! SBT Version ($VERSION) should not contain SNAPSHOT for a release !!!"
	echo
	exit
fi

read -p "Pre Release label (e.g. rc1): " PRE_RELEASE

if [ -z "$PRE_RELEASE" ]
then
	echo
	echo "!!! Pre Release label must not be empty !!!"
	echo
	exit
fi

read -e -p "Path to Apache dist directory: " APACHE_DIST

RELEASE_ROOT_DIR="$APACHE_DIST/dev/incubator/daffodil/"

OUTPUT_DIR=$RELEASE_ROOT_DIR/$VERSION-$PRE_RELEASE

if [ -d "$OUTPUT_DIR" ]
then
	echo
	echo "!!! Output directory already exists: $OUTPUT_DIR !!!"
	echo
	exit
fi

read -p "Signing Key ID (long format): " PGP_SIGNING_KEY_ID
read -p "Apache Username: " APACHE_USERNAME
read -s -p "Apache Password: " APACHE_PASSWD

echo
echo
echo "!!! Making release $VERSION-$PRE_RELEASE in $OUTPUT_DIR !!!"
echo

mkdir -p $OUTPUT_DIR/{src,bin}/

echo "Creating Source Tarball..."
git archive --format=zip --prefix=apache-daffodil-$VERSION-incubating-src/ HEAD > $OUTPUT_DIR/src/apache-daffodil-$VERSION-incubating-src.zip

echo "Building Convenience Binaries and Publishing to Apache Repository..."
sbt \
  "set updateOptions in ThisBuild := updateOptions.value.withGigahorse(false)" \
  "set credentials in ThisBuild += Credentials(\"Sonatype Nexus Repository Manager\", \"repository.apache.org\", \"$APACHE_USERNAME\", \"$APACHE_PASSWD\")" \
  "set publishTo in ThisBuild := Some(\"Apache Staging Distribution Repository\" at \"https://repository.apache.org/service/local/staging/deploy/maven2\")" \
  "set pgpSigningKey := Some(new java.math.BigInteger(\"$PGP_SIGNING_KEY_ID\", 16).longValue)" \
  "+compile" \
  "+publishSigned" \
  "daffodil-cli/rpm:packageBin" \
  "daffodil-cli/universal:packageBin" \
  "daffodil-cli/universal:packageZipTarball"

cp daffodil-cli/target/universal/apache-daffodil-*.tgz $OUTPUT_DIR/bin/
cp daffodil-cli/target/universal/apache-daffodil-*.zip $OUTPUT_DIR/bin/
cp daffodil-cli/target/rpm/RPMS/noarch/apache-daffodil-*.rpm $OUTPUT_DIR/bin/

echo "Calculating Checksums..."
for i in src/ bin/
do
    pushd $OUTPUT_DIR/$i > /dev/null
    for file in *
    do
       sha1sum $file > $file.sha1
       sha256sum $file > $file.sha256
       sha512sum $file > $file.sha512
       gpg --default-key $PGP_SIGNING_KEY_ID --detach-sign --armor --output $file.asc $file
    done
    popd > /dev/null
done

echo "Creating Git tag..."
git tag -as -u $PGP_SIGNING_KEY_ID -m "Release v$VERSION-$PRE_RELEASE"  v$VERSION-$PRE_RELEASE

echo ""
echo ""
echo "!!! Finished: $VERSION-$PRE_RELEASE output to $OUTPUT_DIR !!!"

echo "Things to verify: "
echo
echo "- Files in $RELEASE_ROOT_DIR"
echo "- Staged published files at https://repository.apache.org/"
echo "- Git tag created at v$VERSION-$PRE_RELEASE"
echo
echo "If that all looks corect, 'svn ci' the new files in $RELEASE_ROOT_DIR, close the published Nexus files, 'git push origin v$VERSION-$PRE_RELEASE', and start a VOTE"
echo
