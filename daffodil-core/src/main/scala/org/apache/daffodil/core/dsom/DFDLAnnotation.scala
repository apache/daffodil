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

package org.apache.daffodil.core.dsom

import scala.xml.Node

import org.apache.daffodil.lib.util.Misc

/**
 * Base class for any DFDL annotation
 *
 * Note about SchemaComponent as a base class:
 * Many things are now derived from SchemaComponent that were not before.
 * Just turns out that there is a lot of desirable code sharing between
 * things that aren't strictly-speaking SchemaComponents and things that
 * previously were not. Accomplishing that sharing with mixins and
 * self-typing etc. was just too troublesome. So now many things
 * are schema components. E.g., all annotation objects, the Include
 * and Import objects which represent those statements in a schema,
 * the proxy DFDLSchemaFile object, etc.
 *
 *
 */
abstract class DFDLAnnotation(xmlArg: Node, annotatedSCArg: AnnotatedSchemaComponent)
  extends SchemaComponent
  with NestingLexicalMixin {

  final override val xml = xmlArg

  final override val optLexicalParent = Option(annotatedSCArg)

  final lazy val annotatedSC = annotatedSCArg

  override def toString = diagnosticDebugName

  override protected lazy val diagnosticDebugNameImpl: String = {
    val cn = Misc.getNameFromClass(this)
    val n =
      if (cn.startsWith("DFDL")) {
        val nn = cn.replaceFirst("DFDL", "")
        "dfdl:" + Misc.initialLowerCase(nn)
      } else {
        cn
      }
    n // + "(" + annotatedSC.path + ")"
  }
}
