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

cd $(dirname $0)/../..

rm -rf daffodil-lib/src_managed/*

cd daffodil-propgen/src/main/scala/org/apache/daffodil/propGen

sed -i 's/org.apache.daffodil.api/org.apache.daffodil.lib.api/g' \
    PropertyGenerator.scala

sed -i 's/org.apache.daffodil.api/org.apache.daffodil.lib.api/g' \
    TunableGenerator.scala \
    WarnIDGenerator.scala

sed -i 's/org.apache.daffodil.exceptions/org.apache.daffodil.lib.exceptions/g' \
    PropertyGenerator.scala \
    TunableGenerator.scala \
    WarnIDGenerator.scala

sed -i 's/org.apache.daffodil.schema/org.apache.daffodil.lib.schema/g' \
    PropertyGenerator.scala \
    TunableGenerator.scala \
    WarnIDGenerator.scala

sed -i 's/org.apache.daffodil.util/org.apache.daffodil.lib.util/g' \
    TunableGenerator.scala

sed -i 's/org.apache.daffodil.xml/org.apache.daffodil.lib.xml/g' \
    TunableGenerator.scala
