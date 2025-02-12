<!--
  Licensed to the Apache Software Foundation (ASF) under one or more
  contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
  The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-->

## Daffodil Build Release Container

Daffodil release artifacts are built using GitHub actions. This container can
be used to build those same artifacts for testing or verifying reproducibility.
This can be used for all Daffodil projects, including the Daffodil VS Code
Extension and the SBT plugin.

To build or update the build release container image:

    podman build -t daffodil-build-release containers/build-release/

To use the container image to build a release, run the following from the root
of the project git repository:

    podman run -it --privileged --group-add keep-groups --rm \
      -v $(pwd):/root/project \
      --hostname daffodil.build \
      daffodil-build-release

When run, the container will ask for an optional pre-release label (e.g. rc1)
and the project to build. The resulting artifacts will be placed in the
target/release-artifacts/ directory.
