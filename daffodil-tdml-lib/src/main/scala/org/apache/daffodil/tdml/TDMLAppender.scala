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

package org.apache.daffodil.tdml

import org.apache.logging.log4j.core.appender.AbstractAppender
import org.apache.logging.log4j.core.filter.ThresholdFilter
import org.apache.logging.log4j.core.LogEvent
import org.apache.logging.log4j.core.Filter.Result
import org.apache.logging.log4j.Level
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.logging.log4j.core.config.plugins.{Plugin, PluginFactory}

@Plugin(
    name="CustomAppender",
    category="Core",
    elementType="appender"
)
class TDMLAppender 
 extends AbstractAppender ("org.apache.daffodil.tdml", ThresholdFilter.createFilter(Level.INFO, Result.ACCEPT, Result.DENY), null, false, null) {

    import TDMLAppender._

    override def append(event: LogEvent): Unit = {
        diagnostics = new LogDiagnostic(event.getMessage.getFormattedMessage) +: diagnostics
    }
}

object TDMLAppender{
    var diagnostics: Seq[Diagnostic] = Nil
    var diagnostics2: Seq[String] = Nil

    @PluginFactory
    def createAppender(): TDMLAppender = {
            new TDMLAppender
    }

    def getDiagnostics(): Seq[Throwable] = {
        diagnostics
    }

    def clearDiagnostics(): Unit = {
        diagnostics = Nil
    }
}

class LogDiagnostic(msg: String)
  extends Diagnostic(Nope, Nope, Nope, Maybe(msg)) {
  override def isError = false
  override def modeName = "Log"
}
