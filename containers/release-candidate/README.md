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

## Daffodil Release Candidate Container

To improve reproducibility and to minimize the effects and variability of a
users environment, the Daffodil release container should be used to create
release candidates.

To build the Daffodil release candidate container image:

    podman build -t daffodil-release-candidate /path/to/daffodil.git/containers/release-candidate/

To use the container image to build a release run the following:

    podman run -it --privileged --group-add keep-groups --rm \
      -v ~/.gitconfig:/root/.gitconfig \
      -v ~/.gnupg/:/root/.gnupg/ \
      -v ~/.ssh/:/root/.ssh/ \
      --hostname daffodil.build \
      daffodil-release-candidate

When run, the container will periodically ask for user input (e.g. usernames,
passwords) to sign and publish release files. Upon completion, you will be
asked to verify the release files and, if everything looks good, run a few
commands to push the release candidate out. Note that because the container
will use tools like git, gpg, and ssh, it is necessary to bind mount your local
configuration files into the container with the -v option.
