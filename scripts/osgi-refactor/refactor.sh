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
REFACTOR_DIR=scripts/osgi-refactor

$REFACTOR_DIR/git-restore.sh
scala $REFACTOR_DIR/gen-symbol-table.scala daffodil-* > $REFACTOR_DIR/symbols.txt
$REFACTOR_DIR/rename-dirs.sh
$REFACTOR_DIR/fix-gen.sh
scala $REFACTOR_DIR/fix-imports.scala $REFACTOR_DIR/symbols.txt daffodil-*
scala $REFACTOR_DIR/add-imports.scala .
sbt genProps
rm -f $REFACTOR_DIR/symbols.txt
