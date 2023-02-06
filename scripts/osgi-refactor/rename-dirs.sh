#!/bin/bash -ex

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

cd "$(dirname $0)/../.."
SRC_ROOT=src/main/scala/org/apache/daffodil
TEST_ROOT=src/test/scala/org/apache/daffodil

function move {
    local source_dir=$1
    local target_dir=$2
    local tmp=/tmp/daffodil-refactor

    rm -rf $tmp
    mkdir $tmp
    mv $source_dir/* $tmp
    rmdir $source_dir
    mkdir -p $target_dir
    mv $tmp/* $target_dir
    rm -rf $tmp
}

function rename {
    local lib_name=$1
    local target_dir=$2
    local source_suffix=$3
    (
        cd $lib_name
        move $SRC_ROOT/$source_suffix $SRC_ROOT/$target_dir

        if [ -e $TEST_ROOT/$source_suffix ]; then
            move $TEST_ROOT/$source_suffix $TEST_ROOT/$target_dir
        fi
    )
}

cp scripts/osgi-refactor/Rat.scala project/Rat.scala

rename daffodil-cli cli
rename daffodil-core core
rename daffodil-lib lib
rename daffodil-macro-lib lib
rename daffodil-runtime1 runtime1
rename daffodil-runtime1-layers layers/runtime1 layers
rename daffodil-runtime1-unparser unparsers/runtime1 processors/unparsers
rename daffodil-tdml-processor processor/tdml tdml/processor

mv daffodil-io/src/main/scala/org/apache/daffodil/processors \
   daffodil-io/src/main/scala/org/apache/daffodil/io
mv daffodil-cli/src/test/scala/org/apache/daffodil/cli/CLI/* \
   daffodil-cli/src/test/scala/org/apache/daffodil/cli/cliTest
rmdir  daffodil-cli/src/test/scala/org/apache/daffodil/cli/CLI
mv daffodil-cli/src/test/resources/org/apache/daffodil/CLI \
   daffodil-cli/src/test/resources/org/apache/daffodil/cli
find daffodil-cli/src/test/scala -name "*.scala" -exec sed -i "s_/CLI/_/cli/_g" {} \;
sed -i "s_/CLI/_/cli/_g" project/Rat.scala

mkdir -p daffodil-tdml-lib/src/main/scala/org/apache/daffodil/tak
mv daffodil-tdml-lib/src/main/scala/org/apache/daffodil/Tak.scala \
   daffodil-tdml-lib/src/main/scala/org/apache/daffodil/tak/Tak.scala
sed -i 's/package org.apache.daffodil/package org.apache.daffodil.tak/' daffodil-tdml-lib/src/main/scala/org/apache/daffodil/tak/Tak.scala

sed -i 's/package org.apache.daffodil.tdml/package org.apache.daffodil.processor.tdml/' daffodil-tdml-processor/src/main/scala/org/apache/daffodil/tdml/*.scala
mv daffodil-tdml-processor/src/main/scala/org/apache/daffodil/tdml/*.scala daffodil-tdml-processor/src/main/scala/org/apache/daffodil/processor/tdml
rmdir daffodil-tdml-processor/src/main/scala/org/apache/daffodil/tdml

rmdir daffodil-runtime1-unparser/$SRC_ROOT/processors

move daffodil-tdml-processor/src/test/scala/org/apache/daffodil/tdml \
     daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml
sed -i 's/package org.apache.daffodil.tdml/package org.apache.daffodil.processor.tdml/' \
    daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor/tdml/*
move daffodil-tdml-processor/src/test/java/org/apache/daffodil/tdml \
     daffodil-tdml-processor/src/test/java/org/apache/daffodil/processor/tdml

mv daffodil-macro-lib/src/main/scala/org/apache/daffodil/lib/io \
   daffodil-macro-lib/src/main/scala/org/apache/daffodil/io

mkdir daffodil-macro-lib/src/main/scala/org/apache/daffodil/runtime1
mv daffodil-macro-lib/src/main/scala/org/apache/daffodil/lib/processors \
   daffodil-macro-lib/src/main/scala/org/apache/daffodil/runtime1/processors
sed -i 's/org.apache.daffodil.processors.parsers/org.apache.daffodil.runtime1.processors.parsers/' daffodil-macro-lib/src/main/scala/org/apache/daffodil/runtime1/processors/parsers/PointOfUncertaintyMacros.scala

mv daffodil-lib/src/main/resources/META-INF/services/org.apache.daffodil.api.ValidatorFactory daffodil-lib/src/main/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory
sed -i 's/org.apache.daffodil.validation.XercesValidatorFactory/org.apache.daffodil.lib.validation.XercesValidatorFactory/' daffodil-lib/src/main/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory

mv daffodil-lib/src/test/resources/META-INF/services/org.apache.daffodil.api.ValidatorFactory \
   daffodil-lib/src/test/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory
sed -i 's/org.apache.daffodil.validation/org.apache.daffodil.lib.validation/' daffodil-lib/src/test/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory

mv \
   daffodil-io/src/main/resources/META-INF/services/org.apache.daffodil.processors.charset.BitsCharsetDefinition \
   daffodil-io/src/main/resources/META-INF/services/org.apache.daffodil.io.processors.charset.BitsCharsetDefinition
sed -i 's/org.apache.daffodil.processors.charset/org.apache.daffodil.io.processors.charset/' \
   daffodil-io/src/main/resources/META-INF/services/org.apache.daffodil.io.processors.charset.BitsCharsetDefinition

mv \
   daffodil-runtime1-layers/src/main/resources/META-INF/services/org.apache.daffodil.layers.LayerCompiler \
   daffodil-runtime1-layers/src/main/resources/META-INF/services/org.apache.daffodil.runtime1.layers.LayerCompiler
sed -i 's/org.apache.daffodil.layers/org.apache.daffodil.layers.runtime1/' daffodil-runtime1-layers/src/main/resources/META-INF/services/org.apache.daffodil.runtime1.layers.LayerCompiler

mv \
   daffodil-schematron/src/main/resources/META-INF/services/org.apache.daffodil.api.ValidatorFactory \
   daffodil-schematron/src/main/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory

mv \
   daffodil-japi/src/test/resources/META-INF/services/org.apache.daffodil.api.ValidatorFactory \
   daffodil-japi/src/test/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory

mv \
   daffodil-sapi/src/test/resources/META-INF/services/org.apache.daffodil.api.ValidatorFactory \
   daffodil-sapi/src/test/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory

mv \
   daffodil-test/src/test/resources/META-INF/services/org.apache.daffodil.layers.LayerCompiler \
   daffodil-test/src/test/resources/META-INF/services/org.apache.daffodil.runtime1.layers.LayerCompiler
sed -i 's/org.apache.daffodil.layers/org.apache.daffodil.runtime1.layers/' daffodil-test/src/test/resources/META-INF/services/org.apache.daffodil.runtime1.layers.LayerCompiler

mv \
   daffodil-test/src/test/resources/META-INF/services/org.apache.daffodil.processors.charset.BitsCharsetDefinition \
   daffodil-test/src/test/resources/META-INF/services/org.apache.daffodil.io.processors.charset.BitsCharsetDefinition


mkdir \
   daffodil-test/src/test/scala/org/apache/daffodil/runtime1
mv \
   daffodil-test/src/test/scala/org/apache/daffodil/layers \
   daffodil-test/src/test/scala/org/apache/daffodil/runtime1
sed -i 's/package org.apache.daffodil.layers/package org.apache.daffodil.runtime1.layers/' \
   daffodil-test/src/test/scala/org/apache/daffodil/runtime1/layers/*

sed -i 's_org/apache/daffodil/schema_org/apache/daffodil/lib/schema_' \
   daffodil-propgen/src/test/scala/org/apache/daffodil/propGen/TestPropertyGenerator.scala
sed -i 's_org/apache/daffodil/api_org/apache/daffodil/lib/api_' \
   daffodil-propgen/src/test/scala/org/apache/daffodil/propGen/TestPropertyGenerator.scala

sed -i 's/org.apache.daffodil.validation.XercesValidator/org.apache.daffodil.lib.validation.XercesValidator/' \
    daffodil-lib/src/main/scala/org/apache/daffodil/lib/api/Validator.scala

sed -i 's/org.apache.daffodil.Main/org.apache.daffodil.cli.Main/' \
    daffodil-cli/src/templates/*
