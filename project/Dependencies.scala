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

import sbt._

object Dependencies {

  lazy val common = core ++ infoset ++ test

  lazy val core = Seq(
    "com.lihaoyi" %% "os-lib" % "0.9.0", // for writing/compiling C source files
    "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    "com.ibm.icu" % "icu4j" % "72.1",
    "xerces" % "xercesImpl" % "2.12.2",
    "xml-resolver" % "xml-resolver" % "1.2",
    "commons-io" % "commons-io" % "2.11.0",
    "com.typesafe" % "config" % "1.4.2",
    "org.apache.logging.log4j" %% "log4j-api-scala" % "12.0",
    "org.apache.logging.log4j" % "log4j-api" % "2.19.0",
    "org.apache.logging.log4j" % "log4j-core" % "2.19.0" % "it,test",
  )

  lazy val infoset = Seq(
    "org.jdom" % "jdom2" % "2.0.6.1",
    "com.fasterxml.woodstox" % "woodstox-core" % "6.4.0",
    "com.fasterxml.jackson.core" % "jackson-core" % "2.14.1"
  )

  lazy val cli = Seq(
    "org.fusesource.jansi" % "jansi" % "2.4.0",
    "org.jline" % "jline" % "3.21.0",
    "org.rogach" %% "scallop" % "4.1.0",
    "net.sf.expectit" % "expectit-core" % "0.9.0" % "it,test",
    "org.apache.logging.log4j" % "log4j-core" % "2.19.0",
  )

  lazy val test = Seq(
    "junit" % "junit" % "4.13.2" % "it,test",
    "com.github.sbt" % "junit-interface" % "0.13.3" % "it,test",
    "org.scalacheck" %% "scalacheck" % "1.17.0" % "it,test"
  )

  lazy val schematron = Seq(
    "net.sf.saxon" % "Saxon-HE" % "11.4",
  )

  lazy val exi = Seq(
    "com.siemens.ct.exi" % "exificient" % "1.0.4",
  )
}
