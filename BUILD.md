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

# Build Requirements

Daffodil's build requirements include:

* JDK 8 or higher
* SBT 0.13.8 or higher
* C compiler C99 or higher
* Mini-XML Version 3.0 or higher

You will need the Java Software Development Kit ([JDK]) and the Scala
Build Tool ([SBT]) to build Daffodil, run all tests, create packages,
and more.  You can install the latest [Java 11 LTS][JDK] version and
the latest [SBT] version following their websites' instructions or
install them using your operating system's package manager.

Since Daffodil now has a C backend as well as a Scala backend, you
will need a C compiler supporting the [C99] standard or later and the
[Mini-XML] library.  You can install either [gcc] or [clang] using
your operating system's package manager.  If you can't install the
[Mini-XML] library using your operating system's package manager,
you'll have to build it from source.  We'll tell you how to do that on
Windows but the commands should work on other operating systems too.

You can set your environment variables `CC` and `AR` to the correct
commands (or set them to `true` to disable C compilation altogether)
if you don't want `sbt compile` to call your C compiler with `cc` and
`ar` as the default commands.

## Fedora/CentOS/RHEL

When building on CentOS or RHEL, the [EPEL] repository must be enabled by
following its website's instructions. This is necessary to install the
[libmxml-devel][Mini-XML] package.

You can use the `dnf` package manager to install most of the tools
needed to build Daffodil:

    sudo dnf install clang gcc git java-11-openjdk-devel llvm make mxml-devel pkgconf

If you want to use clang instead of gcc, you'll have to set your
environment variables `CC` and `AR` to the clang binaries' names:

    export CC=clang AR=llvm-ar

However, Fedora has no [sbt][SBT] package in its own repositories.
You'll have to install the latest [SBT] version following its
website's instructions.

Now you can build Daffodil from source and the sbt and daffodil
commands you type will be able to call the C compiler.

## Ubuntu 20.04

You can use the `apt` package manager to install most of the tools
needed to build Daffodil:

    sudo apt install build-essential clang-10 clang-format-10 default-jdk git libmxml-dev

If you want to use clang instead of gcc, you'll have to set your
environment variables `CC` and `AR` to the clang binaries' names:

    export CC=clang-10 AR=llvm-ar-10

However, Ubuntu has no [sbt][SBT] package in its own repositories.
You'll have to install the latest [SBT] version following its
website's instructions.

Now you can build Daffodil from source and the sbt and daffodil
commands you type will be able to call the C compiler.

## Windows 10

Install the latest [Java 11 LTS][JDK] version and the latest [SBT]
version following their websites' instructions.

Install [MSYS2] following its website's instructions and open a new
"MSYS2 MSYS" window.  We'll need its collection of free programs and
libraries.

You can use the `pacman` package manager to install most of the tools
needed to build Daffodil:

    pacman -S clang diffutils gcc git make pkgconf

If you want to use clang instead of gcc, you'll have to set your
environment variables `CC` and `AR` to the clang binaries' names:

    export CC=clang AR=llvm-ar

However, MSYS2 has no [libmxml-devel][Mini-XML] package so you'll have
to build the [Mini-XML] library from source:

    git clone -b v3.3 https://github.com/michaelrsweet/mxml.git
    # some daffodil tests fail if you build mxml with clang
    unset CC AR
    cd mxml
    ./configure --prefix=/usr --disable-shared --disable-threads
    make
    make install

Define an environment variable with the name `MSYS2_PATH_TYPE` and the
value `inherit` using Windows' control panel for editing environment
variables.

Now when you open a new "MSYS2 MSYS" window from the Start Menu to
build Daffodil from source, the sbt and daffodil commands you type
will be able to call the C compiler.

## macOS

Install the Xcode Command Line Tools:

    xcode-select --install

You can use the [Homebrew] package manager to install most of the tools
needed to build Daffodil:

    brew install clang-format
    brew install git
    brew install llvm # needed by iwyu
    brew install include-what-you-use
    brew install libmxml
    brew install openjdk@11
    brew install sbt

Now you can build Daffodil from source and the sbt and daffodil
commands you type will be able to call the C compiler.

[C99]: https://en.wikipedia.org/wiki/C99
[EPEL]: https://docs.fedoraproject.org/en-US/epel/
[Homebrew]: https://brew.sh/
[JDK]: https://adoptopenjdk.net/
[Mini-XML]: https://www.msweet.org/mxml/
[MSYS2]: https://www.msys2.org/
[SBT]: https://www.scala-sbt.org/
[clang]: https://clang.llvm.org/get_started.html
[gcc]: https://linuxize.com/post/how-to-install-gcc-on-ubuntu-20-04/
