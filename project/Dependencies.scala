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
    "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "com.ibm.icu" % "icu4j" % "67.1",
    "xerces" % "xercesImpl" % "2.12.0",
    "xml-resolver" % "xml-resolver" % "1.2",
    "commons-io" % "commons-io" % "2.8.0",
    "jline" % "jline" % "2.14.6",
    "com.typesafe" % "config" % "1.4.0"
  )

  lazy val infoset = Seq(
    "org.jdom" % "jdom2" % "2.0.6",
    "com.fasterxml.woodstox" % "woodstox-core" % "6.2.3",
    "com.fasterxml.jackson.core" % "jackson-core" % "2.11.3"
  )
   
  lazy val cli = Seq( 
    "org.fusesource.jansi" % "jansi" % "1.17.1",
    "org.rogach" %% "scallop" % "3.5.1",
    "net.sf.expectit" % "expectit-core" % "0.9.0" % "it,test"
  )

  lazy val test = Seq(
    "junit" % "junit" % "4.13.2" % "it,test",
    "com.novocode" % "junit-interface" % "0.11" % "it,test",
    "org.scalacheck" %% "scalacheck" % "1.14.3" % "it,test"
  )

  lazy val schematron = Seq(
    "net.sf.saxon" % "Saxon-HE" % "9.9.1-7"
  )
}
