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
(test in IntegrationTest) := (test in IntegrationTest).dependsOn(stage in Compile).value
(testOnly in IntegrationTest) := (testOnly in IntegrationTest).dependsOn(stage in Compile).evaluated
(testQuick in IntegrationTest) := (testQuick in IntegrationTest).dependsOn(stage in Compile).evaluated

executableScriptName := "daffodil"

packageName in Universal := "apache-daffodil-" + version.value + "-incubating-bin" //tarball name
packageName in Linux := executableScriptName.value
packageName in Rpm := "apache-" + executableScriptName.value
packageName in Windows := executableScriptName.value

mappings in Universal ++= Seq(
  baseDirectory.value / "bin.LICENSE" -> "LICENSE",
  baseDirectory.value / "bin.NOTICE" -> "NOTICE",
  baseDirectory.value / ".." / "DISCLAIMER" -> "DISCLAIMER",
  baseDirectory.value / "README.md" -> "README.md",
)

//
// RPM configuration
//
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
name in Windows := "Daffodil" 

// The Windows packager SBT plug-in maps the maintainer variable into
// the WiX ManufacturerFullName field which is displayed in the properties
// dialog box for the executable.
maintainer in Windows := "Apache Daffodil Developers <dev@daffodil.apache.org>"

// The Windows packager SBT plug-in maps the packageSummary variable
// into the WiX productName field. Another strange choice. 
packageSummary in Windows := "Daffodil"

// The Windows packager SBT plug-in limits the length of the
// packageDescription field to a single line. Originally this was a
// full paragraph, as seen in the RPM section, above.
packageDescription in Windows := """Apache Daffodil (incubating) is the open source implementation of the Data Format Description Language (DFDL)""".trim

// Calculate the version number dynamically and pass it in.
// Windows permits up to four numeric values (e.g. 2.1.5.1820)
// where the numbers represent major, minor, patch and build
// respectively. In RPM packaging we add 'incubating', but
// Windows will barf on this. Here we suffix a zero (0) build
// number for snapshot/development/debug builds. A one (1)
// in the build number could be used to differentiate official
// production builds that are destined for release. 
version in Windows := {
  val parts = version.value.split("-", 2)
  val ver = parts(0) // removes snapshot if it exists
  ver + ".0"
}

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
	"-sice:ICE61",
	"-ext", "WixUIExtension",
	"-cultures:en-us",
	"-loc", ((sourceDirectory in Windows).value / "Product_en-us.wxl").toString
	)

// Build an RTF version of the license file for display in the license
// acceptance dialog box. This file will also be sent to the
// printer when and if the user asks for hard copy via the 'print' button.
wixProductLicense := {
  // Make sure the target direcotry exists.
  (target in Windows).value.mkdirs()
  
  // This target file doesn't exist until placed there by the build.
  val targetLicense = (target in Windows).value / "LICENSE.rtf" 
  val sourceLicense = baseDirectory.value / "bin.LICENSE"
  // somehow convert sourceLicense into RTF and store at targetLicense
  val rtfHeader = """{\rtf {\fonttbl {\f0 Arial;}} \f0\fs18"""
  val rtfFooter = """}"""

  val licenseLines = scala.io.Source.fromFile(sourceLicense, "UTF-8").getLines
  val writer = new java.io.PrintWriter(targetLicense, "UTF-8")
  writer.println(rtfHeader)
  licenseLines.foreach { line =>
    writer.println(line + """\line""")
  }
  writer.println(rtfFooter)
  writer.close
  Option(targetLicense)
}
// Use the wixFiles variable to add in the Daffodil-specific dialog
// boxes and sequence.
wixFiles ++= Seq(
  (sourceDirectory in Windows).value / "WixUI_Daffodil.wxs",
  (sourceDirectory in Windows).value / "DisclaimerDlg_Daffodil.wxs"
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
  val banner = <WixVariable Id="WixUIBannerBmp" Value={ ((sourceDirectory in Windows).value / "banner.bmp").toString } />
  val dialog = <WixVariable Id="WixUIDialogBmp" Value={ ((sourceDirectory in Windows).value / "dialog.bmp").toString } />
  
  // Reference the Daffodil-specific User Interface (dialog box) sequence.
  val ui = <UI><UIRef Id="WixUI_Daffodil" /></UI>
  
  // Make sure we abort if we are not installing on Windows 95 or later.
  val osCondition = <Condition Message="!(loc.OS2Old)"><![CDATA[Installed OR (VersionNT >= 400)]]></Condition>

  // Define icons (ID should not be longer than 18 chars and must end with ".exe")
  val icon = Seq(
    <Icon Id="Icon.exe" SourceFile={ ((sourceDirectory in Windows).value / "apache-daffodil.ico").toString } />,
    <Property Id="ARPPRODUCTICON" Value="Icon.exe" />
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
