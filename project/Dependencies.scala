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
    "com.lihaoyi" %% "os-lib" % "0.8.0", // for writing/compiling C source files
    "org.scala-lang.modules" %% "scala-xml" % "2.0.1",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
    "com.ibm.icu" % "icu4j" % "70.1",
    "xerces" % "xercesImpl" % "2.12.1",
    "xml-resolver" % "xml-resolver" % "1.2",
    "commons-io" % "commons-io" % "2.11.0",
    "com.typesafe" % "config" % "1.4.1",
    "org.apache.logging.log4j" %% "log4j-api-scala" % "12.0",
    "org.apache.logging.log4j" % "log4j-api" % "2.17.1",
    "org.apache.logging.log4j" % "log4j-core" % "2.17.1" % "it,test",
  )

  lazy val infoset = Seq(
    "org.jdom" % "jdom2" % "2.0.6.1",
    "com.fasterxml.woodstox" % "woodstox-core" % "6.2.7",
    "com.fasterxml.jackson.core" % "jackson-core" % "2.13.1"
  )

  lazy val cli = Seq(
    "org.fusesource.jansi" % "jansi" % "2.4.0",
    "org.jline" % "jline" % "3.21.0",
    "org.rogach" %% "scallop" % "4.1.0",
    "net.sf.expectit" % "expectit-core" % "0.9.0" % "it,test",
    "org.apache.logging.log4j" % "log4j-core" % "2.17.1",
  )

  lazy val test = Seq(
    "junit" % "junit" % "4.13.2" % "it,test",
    "com.github.sbt" % "junit-interface" % "0.13.2" % "it,test",
    "org.scalacheck" %% "scalacheck" % "1.15.4" % "it,test"
  )

  lazy val schematron = Seq(
    "net.sf.saxon" % "Saxon-HE" % "10.6"
  )
}
