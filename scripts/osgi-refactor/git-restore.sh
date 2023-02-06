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

BASE_DIR="$(dirname $0)/../.."
SRC_ROOT=src/main/scala/org/apache/daffodil
TEST_ROOT=src/test/scala/org/apache/daffodil

cd $BASE_DIR

function clean {
    lib_name=$1
    target_dir=$2

    rm -rf $lib_name/$SRC_ROOT/$target_dir
    rm -rf $lib_name/$TEST_ROOT/$target_dir
}

git restore daffodil-*

clean daffodil-cli cli
clean daffodil-core core
# clean daffodil-io io (SKIP)
clean daffodil-lib lib
clean daffodil-macro-lib lib
clean daffodil-runtime1 runtime1
# clean daffodil-runtime1-layers layers (SKIP)
clean daffodil-runtime1-unparser unparsers

rm -rf daffodil-io/src/{main,test}/scala/org/apache/daffodil/io/{layers,processors}
rm -rf daffodil-runtime1-layers/src/{main,test}/scala/org/apache/daffodil/layers/runtime1
rm -rf daffodil-macro-lib/src/main/scala/org/apache/daffodil/runtime1
rm -rf daffodil-lib/src/main/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory
rm -rf daffodil-runtime1-layers/src/main/resources/META-INF/services/org.apache.daffodil.runtime1.layers.LayerCompiler
rm -rf daffodil-io/src/main/resources/META-INF/services/org.apache.daffodil.io.processors.charset.BitsCharsetDefinition
rm -rf daffodil-lib/src/test/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory
rm -rf daffodil-test/src/test/scala/org/apache/daffodil/runtime1
rm -rf daffodil-japi/src/test/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory
rm -rf daffodil-sapi/src/test/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory
rm -rf daffodil-schematron/src/main/resources/META-INF/services/org.apache.daffodil.lib.api.ValidatorFactory
rm -rf daffodil-test/src/test/resources/META-INF/services/org.apache.daffodil.io.processors.charset.BitsCharsetDefinition
rm -rf daffodil-test/src/test/resources/META-INF/services/org.apache.daffodil.runtime1.layers.LayerCompiler
rm -rf daffodil-tdml-lib/src/main/scala/org/apache/daffodil/tak
rm -rf daffodil-tdml-processor/src/main/scala/org/apache/daffodil/processor
rm -rf daffodil-tdml-processor/src/test/scala/org/apache/daffodil/processor
rm -rf daffodil-tdml-processor/src/test/java/org/apache/daffodil/processor
rm -rf daffodil-cli/src/test/resources/org/apache/daffodil/cli

find . -name "*.refactor.scala" -delete

git restore project/Rat.scala

rm -rf daffodil-lib/src_managed/*
sbt genProps
