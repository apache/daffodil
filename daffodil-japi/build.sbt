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

enablePlugins(GenJavadocPlugin)
enablePlugins(PublishJavadocPlugin)

// Scala Steward may try to update this version to include the Scala version,
// for example 0.18_2.12.15. This is incorrect because the unidoc plugin uses
// crossVersion to figure out the Scala version. This should be set to just the
// version of the genjavadoc plugin, without the Scala version.
unidocGenjavadocVersion := "0.18"

Genjavadoc / sources := (Genjavadoc / sources).value.filterNot { source =>
  source.toString.contains("$") || source.toString.contains("packageprivate")
}
