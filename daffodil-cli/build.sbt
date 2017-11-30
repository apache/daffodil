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

import com.typesafe.sbt.SbtLicenseReport.autoImportImpl._
import com.typesafe.sbt.license.LicenseCategory
import com.typesafe.sbt.license.LicenseInfo
import com.typesafe.sbt.license.DepModuleInfo
import com.typesafe.sbt.license.DepLicense
import com.typesafe.sbt.license.DepModuleInfo
import com.typesafe.sbt.license.Html

enablePlugins(JavaAppPackaging)
enablePlugins(RpmPlugin)

// need 'sbt stage' to build the CLI for cli tests
(test in Test) := (test in Test).dependsOn(stage in Compile).value
(testOnly in Test) := (testOnly in Test).dependsOn(stage in Compile).evaluated
(testQuick in Test) := (testQuick in Test).dependsOn(stage in Compile).evaluated

licenseReportTitle := "Daffodil_Licenses"  // has an underscore since this is used to create the output file

licenseConfigurations := Set("compile")

licenseSelection := Seq(LicenseCategory("NCSA"), LicenseCategory("ICU")) ++ LicenseCategory.all

licenseOverrides := {
  case DepModuleInfo("commons-io", "commons-io", _) => LicenseInfo(LicenseCategory.Apache, "The Apache Software License, Version 2.0", "http://www.apache.org/licenses/LICENSE-2.0.html")
  case DepModuleInfo("xml-resolver", "xml-resolver", _) => LicenseInfo(LicenseCategory.Apache, "The Apache Software License, Version 2.0", "http://www.apache.org/licenses/LICENSE-2.0.html")
  case DepModuleInfo("org.fusesource.jansi", "jansi", _) => LicenseInfo(LicenseCategory.Apache, "The Apache Software License, Version 2.0", "http://www.apache.org/licenses/LICENSE-2.0.html")
  case DepModuleInfo("com.fasterxml.jackson.core", "jackson-core" , _) => LicenseInfo(LicenseCategory.Apache, "The Apache Software License, Version 2.0", "http://www.apache.org/licenses/LICENSE-2.0.html")
  case DepModuleInfo("com.ibm.icu", "icu4j" , "51.1") => LicenseInfo(LicenseCategory("ICU"), "ICU License", "https://ssl.icu-project.org/repos/icu/tags/release-51-2/icu4j/main/shared/licenses/license.html")
}

licenseFilter := {
  case LicenseCategory("NCSA", _) => false
  case _ => true
}

licenseReportMakeHeader := {
  case Html => Html.header1(licenseReportTitle.value.replace("_", " ")) + "<p>Daffodil is licensed under the <a href='http://opensource.org/licenses/NCSA'>University of Illinois/NCSA Open Source License</a>.</p><p>Below are the libraries that Daffodil depends on and their licenses.<br></p>"
  case l => l.header1(licenseReportTitle.value.replace("_", " "))
}

updateLicenses := {
  val report = updateLicenses.value
  val unmanaged_licenses = Seq(
    DepLicense(DepModuleInfo("passera", "passera", "0.1"), LicenseInfo(LicenseCategory.BSD, "BSD", "https://github.com/nystrom/scala-unsigned/blob/master/BSD-LICENSE.txt"), Set("runtime"))
  )
  report.copy(licenses = report.licenses ++ unmanaged_licenses)
}

executableScriptName := "daffodil"

packageName in Universal := "daffodil-" + version.value + "-bin" //tarball name

packageName in Linux := executableScriptName.value

packageName in Rpm := executableScriptName.value

mappings in Universal ++= Seq(
  dumpLicenseReport.value / (licenseReportTitle.value + ".html") -> "LICENSES.html",
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
  parts(0) + ".incubating" // removes snapshot/beta/rc/etc, that should only be in the rpmRelease
}

rpmRelease := {
  val parts = version.value.split("-", 2) // parts(0) is the version, parse(1) is snapshot/beta/rc/etc if it exists
  if (parts.length > 1) "0." + parts(1).toLowerCase else "1"
}

rpmLicense := Some(licenses.value.map { case (n: String, _) => n }.mkString(" and "))

rpmPrefix := Some(defaultLinuxInstallLocation.value)
