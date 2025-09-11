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

import sbt.*

object Dependencies {

  lazy val common = core ++ infoset ++ test

  lazy val core = Seq(
    "com.lihaoyi" %% "os-lib" % "0.11.5", // for writing/compiling C source files
    "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    "com.ibm.icu" % "icu4j" % "77.1",
    "xerces" % "xercesImpl" % "2.12.2",
    "xml-resolver" % "xml-resolver" % "1.2",
    "commons-io" % "commons-io" % "2.20.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
  )

  lazy val slf4jAPI = Seq(
    "org.slf4j" % "slf4j-api" % "2.0.17"
  )

  lazy val infoset = Seq(
    "org.jdom" % "jdom2" % "2.0.6.1",
    "com.fasterxml.woodstox" % "woodstox-core" % "7.1.1",
    "com.fasterxml.jackson.core" % "jackson-core" % "2.16.1"
  )

  lazy val cli = Seq(
    "org.jline" % "jline" % "3.30.6",
    "org.rogach" %% "scallop" % "5.2.0",
    "net.sf.expectit" % "expectit-core" % "0.9.0" % "test"
  )

  lazy val junit = Seq(
    "junit" % "junit" % "4.13.2"
  )

  lazy val test = Seq(
    "junit" % "junit" % "4.13.2" % "test",
    "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
    "org.scalacheck" %% "scalacheck" % "1.19.0" % "test"
  )

  lazy val schematron = Seq(
    "net.sf.saxon" % "Saxon-HE" % "12.8"
  )

  lazy val exi = Seq(
    "com.siemens.ct.exi" % "exificient" % "1.0.7"
  )
}
