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

package org.apache.daffodil.japi.packageprivate
// The compiler plugin that converts Scala code to Java code for Javadoc
// generation has no concept of package private. So if there is anything that
// should be package private and thus not part of the public API (e.g.
// utilities for converting from Scala to Java), there isn't a way to exclude
// them from Javadocs. So, when we generate Javadocs, we manually exclude
// anything in the japiexclude package. So anything that should be package
// private should go in this package.
import org.apache.daffodil.japi._
import org.apache.daffodil.japi.debugger._
import org.apache.daffodil.japi.infoset._
import org.apache.daffodil.lib.iapi.{ ValidationMode => SValidationMode }
import org.apache.daffodil.runtime1.debugger.{ InteractiveDebugger => SInteractiveDebugger }
import org.apache.daffodil.runtime1.debugger.{
  InteractiveDebuggerRunner => SInteractiveDebuggerRunner
}
import org.apache.daffodil.runtime1.infoset.{ XMLTextEscapeStyle => SXMLTextEscapeStyle }

private[japi] object ValidationConversions {

  def modeToScala(mode: ValidationMode): SValidationMode.Type = {
    val smode: SValidationMode.Type = mode match {
      case ValidationMode.Off => SValidationMode.Off
      case ValidationMode.Limited => SValidationMode.Limited
      case ValidationMode.Full => SValidationMode.Full
    }
    smode
  }
}

private[japi] object XMLTextEscapeStyleConversions {

  def styleToScala(style: XMLTextEscapeStyle): SXMLTextEscapeStyle.Value = {
    val sxmlTextEscapeStyle: SXMLTextEscapeStyle.Value = style match {
      case XMLTextEscapeStyle.Standard => SXMLTextEscapeStyle.Standard
      case XMLTextEscapeStyle.CDATA => SXMLTextEscapeStyle.CDATA
      case null =>
        throw new Exception(
          "Unrecognized value: %s for parameter: xmlTextEscapeStyle. Must be 'Standard' or 'CDATA'."
            .format(style)
        )
    }
    sxmlTextEscapeStyle
  }
}

/* A wrapper interctive debugger that scala debugging can talk to, which is
 * then forwarded onto the java interactive debugger, if a user implements
 * their own debugger in java.
 */
private[japi] class JavaInteractiveDebuggerRunner(dr: DebuggerRunner)
  extends SInteractiveDebuggerRunner {
  def init(id: SInteractiveDebugger): Unit = dr.init()
  def getCommand: String = dr.getCommand()
  def lineOutput(line: String): Unit = dr.lineOutput(line)
  def fini(): Unit = dr.fini()
}
