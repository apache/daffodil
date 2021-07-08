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

import scala.xml.Attribute
import scala.xml.transform.RewriteRule
import scala.xml.transform.RuleTransformer

enablePlugins(JavaAppPackaging)
enablePlugins(RpmPlugin)
enablePlugins(WindowsPlugin)

// need 'sbt stage' to build the CLI for cli integration tests
(IntegrationTest / test) := (IntegrationTest / test).dependsOn(Compile / stage).value
(IntegrationTest / testOnly) := (IntegrationTest / testOnly).dependsOn(Compile / stage).evaluated
(IntegrationTest / testQuick) := (IntegrationTest / testQuick).dependsOn(Compile / stage).evaluated

executableScriptName := "daffodil"

Universal / packageName := "apache-daffodil-" + version.value + "-bin" //tarball name
Linux / packageName := executableScriptName.value
Rpm / packageName := "apache-" + executableScriptName.value
Windows / packageName := executableScriptName.value

Universal / mappings ++= Seq(
  baseDirectory.value / "bin.LICENSE" -> "LICENSE",
  baseDirectory.value / "bin.NOTICE" -> "NOTICE",
  baseDirectory.value / "README.md" -> "README.md",
  sourceDirectory.value / "conf" / "log4j2.xml" -> "conf/log4j2.xml",
)

maintainer := "Apache Daffodil <dev@daffodil.apache.org>"

//
// RPM configuration
//
rpmVendor := "Apache Daffodil"

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
// In this case, we want to disable zstd compression which isn't supported by
// older versions of RPM. So we add the following special rpm %define's to use
// gzip compression instead, which is supported by all versions of RPM.
Rpm / packageDescription := (Rpm / packageDescription).value + """
%define _source_payload w9.gzdio
%define _binary_payload w9.gzdio
"""

Rpm / version := {
  val parts = version.value.split("-", 2)
  val ver = parts(0) // removes snapshot if it exists
  ver
}

rpmRelease := {
  val parts = version.value.split("-", 2) // parts(0) is the version, parse(1) is snapshot if it exists
  if (parts.length > 1) "0." + parts(1).toLowerCase else "1"
}

rpmLicense := Some(licenses.value.map { case (n: String, _) => n }.mkString(" and "))

rpmPrefix := Some(defaultLinuxInstallLocation.value)

//
// Windows configuration
//

//
// Here we set the variables that are supported by the SBT Native Packager plug-in.
// We also get fairly aggressive in editing/modifying the XML in order
// to control and use some specific features that are supported by WiX
// but which are not properly suported by the SBT plug-in.
//

// Force the correct installation directory name. This overwrites
// 'daffodil-cli', which is the directory that we invoke sbt in.
// The SBT WiX plug-in incorrectly assumes that the directory of
// invocation is the same name as the direcotry you eventually
// want to install into.
Windows / name := "Daffodil"

// The Windows packager SBT plug-in maps the packageSummary variable
// into the WiX productName field. Another strange choice.
Windows / packageSummary := "Daffodil"

// The Windows packager SBT plug-in limits the length of the packageDescription
// field to a single line. Use the short packageSummary from the RPM config.
Windows / packageDescription := (Rpm / packageSummary).value

// Use the same version number as in the rpm, which has SNAPSHOT removed if it
// exists. Windows version numbers has no concept of a "snapshot build", only
// major, minor, patch, and build. So Windows MSI versions do not differentiate
// between snapshots and non-snapshots.
Windows / version := (Rpm / version).value

// Required and critical GUIDs. Ironically the ProductId is unique
// to a given release, but UpgradeId must NEVER change! This may
// seem conter-intuitive, but the UpgradeId is actually what ties
// the product to it's upgrades and the product is actually unique
// each time it is released, so there is some semblance of logic
// to this scheme.
wixProductUpgradeId := "4C966AFF-585E-4E17-8CC2-059FD70FEC77"

// Light options. Bring in standard dialog boxes and localization.
// The suppression of ICE61 is required as we *DO* permit
// re-installation of the same version. Despite the presence of
// specific XML to enable this, the WiX compiler and linker
// complain about it unless you specifically suppress the warning.
lightOptions := Seq(
  "-sval", // validation does not currently work under Wine, this disables that
  "-sice:ICE61",
  "-ext", "WixUIExtension",
  "-cultures:en-us",
  "-loc", ((Windows / sourceDirectory).value / "Product_en-us.wxl").toString
)

// Build an RTF version of the license file for display in the license
// acceptance dialog box. This file will also be sent to the
// printer when and if the user asks for hard copy via the 'print' button.
wixProductLicense := {
  // Make sure the target direcotry exists.
  (Windows / target).value.mkdirs()

  // This target file doesn't exist until placed there by the build.
  val targetLicense = (Windows / target).value / "LICENSE.rtf"
  val sourceLicense = baseDirectory.value / "bin.LICENSE"
  // somehow convert sourceLicense into RTF and store at targetLicense
  val rtfHeader = """{\rtf {\fonttbl {\f0 Arial;}} \f0\fs18"""
  val rtfFooter = """}"""

  val licenseLines = scala.io.Source.fromFile(sourceLicense, "UTF-8").getLines
  val writer = new java.io.PrintWriter(targetLicense, "UTF-8")
  // windows style line endings in the license are required by the WiX toolkit
  writer.write(rtfHeader + "\r\n")
  licenseLines.foreach { line =>
    writer.write(line + """\line""" + "\r\n")
  }
  writer.write(rtfFooter + "\r\n")
  writer.close
  Option(targetLicense)
}

// Use the wixFiles variable to add in the Daffodil-specific dialog
// boxes and sequence.
wixFiles ++= Seq(
  (Windows / sourceDirectory).value / "WixUI_Daffodil.wxs",
)

// The SBT Native Packager plug-in assumes that we want to give the user
// a Feature Tree to select from. One of the 'features' that the plug-in
// offers up is a set of all shortcuts and menu links. Daffodil is
// actually a command-line executable, so we do not include
// configuration links as they are unnecessary. From a practical
// standpoint the user must invoke a command shell in a window
// before they can invoke Daffodil anyway.
wixFeatures := {
  val features = wixFeatures.value
  features.filter { _.id != "AddConfigLinks"}
}

// Make sure that we don't use an MSI installer that is older than
// version 2.0. It also fixes the comment attribute that hangs
// out on the Package keyword.
wixPackageInfo := wixPackageInfo.value.copy(installerVersion = "200", comments = "!(loc.Comments)")

// Fix the XML that is associated with the installable files and directories.
wixProductConfig := {
  // Pick up the generated code.
  val pc = wixProductConfig.value

  // Replace the default headline banner and Welcome/Exit screen
  // bitmaps with the custom ones we developed for Daffodil.
  val banner = <WixVariable Id="WixUIBannerBmp" Value={ ((Windows / sourceDirectory).value / "banner.bmp").toString } />
  val dialog = <WixVariable Id="WixUIDialogBmp" Value={ ((Windows / sourceDirectory).value / "dialog.bmp").toString } />

  // Reference the Daffodil-specific User Interface (dialog box) sequence.
  val ui = <UI><UIRef Id="WixUI_Daffodil" /></UI>

  // Make sure we abort if we are not installing on Windows 95 or later.
  val osCondition = <Condition Message="!(loc.OS2Old)"><![CDATA[Installed OR (VersionNT >= 400)]]></Condition>

  // Define icons (ID should not be longer than 18 chars and must end with ".exe")
  val icon = Seq(
    <Icon Id="Daffodil.ico" SourceFile={ ((Windows / sourceDirectory).value / "apache-daffodil.ico").toString } />,
    <Property Id="ARPPRODUCTICON" Value="Daffodil.ico" />
  )

  // String together the additional XML around the generated directory and file lists.
  val pcGroup = pc.asInstanceOf[scala.xml.Group]
  val newNodes = osCondition ++ icon ++ pcGroup.nodes ++ dialog ++ banner ++ ui
  val pcWithNewNodes = pcGroup.copy(nodes = newNodes)

  // Change (edit) some items inside the directory/files list.
  val pcRewriteRule = new RewriteRule {
    override def transform(n: scala.xml.Node): Seq[scala.xml.Node] = n match {

      // We want to comply with the Windows standard pattern of
      // installing at /Program Files/ManufacturerName/Application
      // This case effectively inserts the manufacturer name into
      // the XML as a directory to comply with the standard.
      case e: scala.xml.Elem if (e \ "@Name").text == "PFiles" => {
        val apacheDir = <Directory Id="ProgramFilesApache" Name="!(loc.ManufacturerName)" />
        val apacheDirWithChild = apacheDir.copy(child = e.child)
        e.copy(child = apacheDirWithChild)
      }

      // We *ARE* going to allow the user to repair and reinstall
      // the same exact version, so we need to add an attribute
      // to the MajorUpgrade keyword.  This will trigger an 'ICE61'
      // error that we suppress on the 'light' linker command line.
      case e: scala.xml.Elem if e.label == "MajorUpgrade" => {
        e % scala.xml.Attribute("", "AllowSameVersionUpgrades", "yes", e.attributes)
      }

      // Fixup for registry key.
      case e: scala.xml.Elem if e.label == "RegistryValue" => {
        val attribs = e.attributes.remove("Key")
        e % scala.xml.Attribute("", "Key", """Software\Apache\Installed Products\Daffodil""", attribs)
      }

      // The WixUI_FeatureTree reference has to be removed so that
      // our custom Daffodil UI can operate properly.
      case e: scala.xml.Elem if e.label == "UIRef" && (e \ "@Id").text == "WixUI_FeatureTree" => {
        scala.xml.NodeSeq.Empty
      }
      case `n` => n
    }
  }

  // Now apply all the edits in the RewriteRule defined above.
  val newXml = new RuleTransformer(pcRewriteRule).transform(pcWithNewNodes)
  <xml:group>{newXml}</xml:group>
}
