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

addCompilerPlugin("com.typesafe.genjavadoc" %% "genjavadoc-plugin" % "0.16" cross CrossVersion.full)


lazy val JavaDoc = config("genjavadoc") extend Compile

configs(JavaDoc)

inConfig(JavaDoc)(Defaults.configSettings)

scalacOptions += "-P:genjavadoc:out=" + target.value + "/java"

packageDoc in Compile := (packageDoc in JavaDoc).value

sources in JavaDoc :=
  (target.value / "java" ** "*.java").get.filterNot(f => f.toString.contains("$") || f.toString.contains("packageprivate")) ++
  (sources in Compile).value.filter(_.getName.endsWith(".java"))

javacOptions in JavaDoc := Seq(
  "-windowtitle", "Apache Daffodil (incubating) " + version.value + " Java API",
  "-doctitle", "<h1>Apache Daffodil (incubating) " + version.value + " Java API</h1>"
)

artifactName in packageDoc in JavaDoc := ((sv, mod, art) => "" + mod.name + "_" + sv.binary + "-" + mod.revision + "-javadoc.jar")
