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
* Mini-XML Version 3.2 or higher

You will need the Java Software Development Kit ([JDK]) and the Scala
Build Tool ([SBT]) to build Daffodil, run all tests, create packages,
and more.  The recommended way is to install the latest [Java 11
LTS][JDK] version and the latest [SBT] version directly from their
websites.  A package manager on your operating system may be a more
convenient way, but it may install an older version than what you'd
prefer.

Since Daffodil now has a C backend as well as a Scala backend, you
will need a C compiler ([gcc] or [clang]), the [Mini-XML] library, and
possibly the GNU [argp] library if your system's C library doesn't
include it already.  The recommended way is to install them using your
operating system's package manager (for example, `apt` on Debian or
Ubuntu):

    sudo apt install build-essential curl git libmxml-dev
    # Just listing other packages you might need too

Some operating systems don't package the [Mini-XML] library, so you
may have to build and install it from source on those operating
systems.  See the Windows commands below for how to do so.

You can set your environment variable "CC" to the appropriate driver
command (or set it to `true` to disable C compilation altogether) if
you don't want `sbt compile` to call your C compiler with `cc` as the
driver command.

On Windows, the easiest way to install gcc and libargp is to install
[MSYS2]'s collection of free tools and libraries although MSYS2 has no
package for libmxml which you'll need to build from source.  First
install [MSYS2] following its website's installation instructions,
then run the following commands in a "MSYS2 MSYS" window:

    pacman -S gcc git libargp-devel make pkgconf
    git clone https://github.com/michaelrsweet/mxml.git
    cd mxml
    ./configure --prefix=/usr --disable-shared --disable-threads
    make
    make install

You'll also need to define an environment variable using Windows'
control panel for editing environment variables.  Define an
environment variable with the name `MSYS2_PATH_TYPE` and the value
`inherit`.  Now when you open a new "MSYS2 MSYS" window from the Start
Menu, you will be able to type sbt and daffodil commands in the MSYS2
window and both sbt and daffodil will be able to call the C compiler.

[JDK]: https://adoptopenjdk.net/
[Mini-XML]: https://www.msweet.org/mxml/
[MSYS2]: https://www.msys2.org/
[SBT]: https://www.scala-sbt.org/
[argp]: https://packages.msys2.org/package/libargp-devel
[clang]: https://clang.llvm.org/get_started.html
[gcc]: https://linuxize.com/post/how-to-install-gcc-on-ubuntu-20-04/
