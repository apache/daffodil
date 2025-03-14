/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import scala.sys.process.Process

import sbt.internal.util.Util.isWindows

enablePlugins(JavaAppPackaging)
enablePlugins(RpmPlugin)

lazy val packageWindowsBin = taskKey[Unit]("Generate windows installer")
lazy val isccPath = settingKey[String]("Path to the Inno Setup ISCC.exe file")

executableScriptName := "daffodil"

Universal / packageName := "apache-daffodil-" + version.value + "-bin" //tarball name
Linux / packageName := executableScriptName.value
Rpm / packageName := "apache-" + executableScriptName.value

val optSourceDateEpoch = scala.util.Properties.envOrNone("SOURCE_DATE_EPOCH")

// prepend additional options to the tar command for reproducibility. We prepend because the
// default value of this setting includes the -f option at the end, which needs to stay at the
// end since sbt-native-packager provides the archive file immediately after
Universal / packageZipTarball / universalArchiveOptions := {
  val optMtime = optSourceDateEpoch.map { epoch =>
    val fmt = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ssZ")
    fmt.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    val mtime = fmt.format(new java.util.Date(epoch.toLong * 1000))
    s"--mtime=$mtime"
  }
  val newOptions = Seq(
    "--sort=name",
    "--owner=0",
    "--group=0",
    "--numeric-owner"
  ) ++ optMtime
  newOptions ++ (Universal / packageZipTarball / universalArchiveOptions).value
}

Universal / mappings ++= Seq(
  baseDirectory.value / "bin.LICENSE" -> "LICENSE",
  baseDirectory.value / "bin.NOTICE" -> "NOTICE",
  baseDirectory.value / "README.md" -> "README.md"
)

// When the sbt-native-packger plugin creates a tar/zip/RPM/etc, it does not include the
// crossVersion in mapping of jars built from this project (i.e. org.apache.daffodil jars only).
// There are cases where this crossVersion would be useful (e.g. consistency with Ivy jar names
// that do include the crossVersion), so this modifies the mapping to include that. Note that
// the name of the path already has what we need, we just need to prepend "lib/" and our
// organization.
Universal / mappings := (Universal / mappings).value.map { case (path, mapping) =>
  val thisProjectJarPrefix = "lib/" + organization.value + "."
  val newMapping =
    if (mapping.startsWith(thisProjectJarPrefix))
      thisProjectJarPrefix + path.getName
    else
      mapping
  (path, newMapping)
}

maintainer := "Apache Daffodil <dev@daffodil.apache.org>"

//
// RPM configuration
//
rpmVendor := "Apache Daffodil"

// Add an alias so users can install either "apache-daffodil" or "daffodil" with dnf
rpmProvides := Seq("daffodil = %{version}-%{release}")

Rpm / packageArchitecture := "noarch"

Rpm / packageSummary := "Open-source implementation of the Data Format Description Language (DFDL)"

Rpm / packageDescription := """
Apache Daffodil is an open-source implementation of the DFDL specification
that uses DFDL data descriptions to parse fixed format data into an infoset.
This infoset is commonly converted into XML or JSON to enable the use of
well-established XML or JSON technologies and libraries to consume, inspect,
and manipulate fixed format data in existing solutions. Daffodil is also
capable of serializing or "unparsing" data back to the original data format.
The DFDL infoset can also be converted directly to/from the data structures
carried by data processing frameworks so as to bypass any XML/JSON overheads.
""".trim

// The below is a hack. The sbt native packager plugin does not provide a way
// to make arbitrary changes to the RPM spec file. However, RPM spec files
// allow %define's within a description. So if we need to add defines to change
// rpmbuild behavior, we can simply append them to the RPM description and
// things still work as expected.
//
// Older versions of RPM do not support zstd compression. To disable this we can
// define _source_payload and _binary_payload to use gzip compression.
// Additionally, the bulk of the RPM is jars which are already compressed and
// won't really compress any further, so we set the compression level to zero
// for faster builds.
//
// _buildhost is set to ensure reproducible builds regardless of the hostname of
// the system where where we are building the RPM.
//
// optflags is set to empty for reproducible builds--different systems use
// different values of optflags and store the value in the RPM metadata. It
// doesn't matter that we set it to nil because the macro is only used for
// things like CFLAGS, CXXFLAGS, etc. and the way use rpmbuild it does not use
// this flags, since it just packages files already built by SBT.
//
// Even with these above settings, different systems still might create RPMs
// with different internal tags. For example, the CLASSDICT and FILECLASS tags
// cannot be controlled by the spec file, and include human readable
// descriptions of each installed file. These descriptions are created by
// libmagic and can differ depending on the version of libmagic on a system. RPM
// also includes a PLATFORM tag that usually includes the distribution (e.g.
// redhat vs debian), again something the spec file cannot change. And RPM also
// includes the version of RPM used to build the RPM file. All that to say that
// although we can minimze differences by changing some macros, the same or very
// similar environment is still needed for byte exact reproducible RPM builds.
// However, this is usually enough for the rpmdiff tool to report no differences
// since it doesn't look at tags that don't really matter.
Rpm / packageDescription := (Rpm / packageDescription).value + """
%define _source_payload w0.gzdio
%define _binary_payload w0.gzdio
%define _buildhost daffodil.build
%define optflags %{nil}
"""

Rpm / version := {
  val parts = version.value.split("-", 2)
  val ver = parts(0) // removes snapshot if it exists
  ver
}

rpmRelease := {
  val parts =
    version.value.split("-", 2) // parts(0) is the version, parse(1) is snapshot if it exists
  if (parts.length > 1) "0." + parts(1).toLowerCase else "1"
}

rpmLicense := Some(licenses.value.map { case (n: String, _) => n }.mkString(" and "))

rpmPrefix := Some(defaultLinuxInstallLocation.value)

//
// Windows configuration
//

/**
 * If building on a non-Windows machine, uses winepath to convert a Unix file path to a Windows
 * path that can be used with wine. If this is Windows, just return the file path as a string
 */
def winpath(file: File): String = {
  if (isWindows) file.toString
  else Process(Seq("winepath", "-w", file.toString)).!!
}

/**
 * Set the default path to the ISCC compiler to the Windows path. Since we run it with wine on
 * Linux, this path should work on both Windows and Linux. Depending on the wine config, windows
 * version, or how Inno Setup is installed, this might need to be set to a different value.
 */
isccPath := "C:\\Program Files (x86)\\Inno Setup 6\\ISCC.exe"

packageWindowsBin := {
  (Universal / stage).value

  // if SOURCE_DATE_EPOCH is defined, then we use that as the TouchDate/Time settings to set
  // embedded timestamps, which allows for reproducible builds
  val (touchDate, touchTime) = optSourceDateEpoch
    .map { epoch =>
      val fmt = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
      fmt.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
      val time = fmt.format(new java.util.Date(epoch.toLong * 1000))
      val dateTime = time.split(" ")
      (dateTime(0), dateTime(1))
    }
    .getOrElse(("current", "current"))

  val optWine = if (!isWindows) Some("wine") else None
  val isccCmd = optWine ++: Seq(
    isccPath.value,
    "/O" + winpath(target.value / "windows"),
    "/F" + (Universal / packageName).value,
    "/DVERSION=" + version.value,
    "/DBASEDIR=" + winpath(baseDirectory.value),
    "/DTOUCHDATE=" + touchDate,
    "/DTOUCHTIME=" + touchTime,
    winpath(baseDirectory.value / "src" / "windows" / "apache-daffodil.iss")
  )
  Process(isccCmd).!!
}
