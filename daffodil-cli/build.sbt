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

enablePlugins(JavaAppPackaging)
enablePlugins(RpmPlugin)

// need 'sbt stage' to build the CLI for cli tests
(test in Test) := (test in Test).dependsOn(stage in Compile).value
(testOnly in Test) := (testOnly in Test).dependsOn(stage in Compile).evaluated
(testQuick in Test) := (testQuick in Test).dependsOn(stage in Compile).evaluated

executableScriptName := "daffodil"

packageName in Universal := "apache-daffodil-" + version.value + "-incubating-bin" //tarball name

packageName in Linux := executableScriptName.value

packageName in Rpm := "apache-" + executableScriptName.value

mappings in Universal ++= Seq(
  baseDirectory.value / "LICENSE" -> "LICENSE",
  baseDirectory.value / "NOTICE" -> "NOTICE",
  baseDirectory.value / ".." / "DISCLAIMER" -> "DISCLAIMER",
  baseDirectory.value / "README.md" -> "README.md",
)

rpmVendor := "Apache Daffodil"

maintainer in Rpm := "Apache Daffodil <dev@daffodil.apache.org>"

packageArchitecture in Rpm := "noarch"

packageSummary in Rpm := "Open source implementation of the Data Format Description Language (DFDL)"

packageDescription in Rpm := """
Apache Daffodil (incubating) is the open source implementation of the Data
Format Description Language (DFDL), a specification created by the Open Grid
Forum. DFDL is capable of describing many data formats, including textual and
binary, commercial record-oriented, scientific and numeric, modern and legacy,
and many industry standards. It leverages XML technology and concepts, using a
subset of W3C XML schema type system and annotations to describe such data.
Daffodil uses this description to parse data into an XML infoset for ingestion
and validation.
""".trim

version in Rpm := {
  val parts = version.value.split("-", 2)
  val ver = parts(0) // removes snapshot if it exists
  ver + ".incubating"
}

rpmRelease := {
  val parts = version.value.split("-", 2) // parts(0) is the version, parse(1) is snapshot if it exists
  if (parts.length > 1) "0." + parts(1).toLowerCase else "1"
}

rpmLicense := Some(licenses.value.map { case (n: String, _) => n }.mkString(" and "))

rpmPrefix := Some(defaultLinuxInstallLocation.value)
