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
import sbt.Keys._
import sbt.KeyRanks._

object OsgiCheckPlugin extends AutoPlugin {

  class OsgiCheckException()
    extends RuntimeException("OSGI check failed")
      with FeedbackProvidedException

  object autoImport {
    val osgiOwnedPackages = taskKey[Set[String]]("Get all packages owned by this project").withRank(BTask)
    val osgiOwnedPackagesWithOwner = taskKey[(Set[String], String)]("Get a tuple of the osgiOwnedPackages and the project name that owns them").withRank(Invisible)
    val osgiCheck = taskKey[Unit]("Check all subpackages for osgi conflicts").withRank(ATask)
  }

  import autoImport._

  private val packageMatcher = """^\s*package (\S+);?.*$""".r

  override def trigger = allRequirements

  val allSubprojectsFilter = ScopeFilter(inAggregates(ThisProject, includeRoot=false))

  override def projectSettings: Seq[Def.Setting[_]] = {Seq(
    osgiOwnedPackages := {
      val logger = streams.value.log
      (Compile / sources).value.flatMap { file =>
        var source = scala.io.Source.fromFile(file)
        val optPackage = source.getLines.collectFirst {
          case packageMatcher(packageName) => packageName
        }
        if (optPackage.isEmpty) {
          logger.warn(s"${file.getPath}: could not find package statement")
        }
        optPackage
      }.toSet
    },
    osgiOwnedPackagesWithOwner := {
      (osgiOwnedPackages.value, name.value)
    },
    osgiCheck := {
      val logger = streams.value.log

      // create a List of tuples of all osgiOwnedPackages and their owner from all
      // subprojects. If a subproject disables this plugin, then it contributes tuple
      // of empty set and empty string and will be ignored
      val subprojectPackagesAndOwner = (osgiOwnedPackagesWithOwner ?? (Set.empty -> ""))
        .all(allSubprojectsFilter).value

      // flatten all of our tuples so we have a single list of package name ->
      // project owner tuples. At this point, there might be multiple owners
      // for the same package name in this list
      val packageOwnerTuples = subprojectPackagesAndOwner.flatMap { case (ownedPackages, packageOwner) =>
        ownedPackages.map { _ -> packageOwner }
      }

      // create a map, grouping with a key of package name and value of all of the
      // owners that claim to own it. If only one project owns a package, the value
      // should be a list of one.
      val packageOwnersMap = packageOwnerTuples
        .groupBy { case (packageName, _) => packageName }
        .mapValues { list => list.map { case (_, packageOwner) => packageOwner } }

      // find any packages with multiple project owners and error
      val multipleOwners = packageOwnersMap.filter { case (_, owners) => owners.length > 1 }
      if (multipleOwners.size > 0) {
        logger.err("Packages found owned by multiple projects, violating OSGI:")
        multipleOwners.foreach { case (packageName, owners) =>
          logger.err(s"- package $packageName: ${owners.mkString(", ")}")
        }
        throw new OsgiCheckException()
      }
    }
  )}

  override val globalSettings: Seq[Setting[_]] = {Seq(
    // the osgiCheck does aggregation itself by inspecting all subproject, it does
    // not need to be aggregated
    osgiCheck / aggregate := false
  )}

}