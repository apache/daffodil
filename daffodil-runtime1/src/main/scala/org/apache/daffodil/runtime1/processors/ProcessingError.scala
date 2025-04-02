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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.iapi.ThinDiagnostic
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

abstract class ProcessingError protected (
  override val modeName: String,
  schemaContext: Maybe[SchemaFileLocation],
  dataContext: Maybe[DataLocation],
  val maybeCause: Maybe[Throwable], // use this OR the format string, Not both.
  val maybeFormatString: Maybe[String],
  val args: Any*
) extends ThinDiagnostic(schemaContext, dataContext, maybeCause, maybeFormatString, args: _*) {

  override def isError = true

  def this(
    modeName: String,
    rd: Maybe[SchemaFileLocation],
    loc: Maybe[DataLocation],
    fmtString: String,
    args: Any*
  ) = this(modeName, rd, loc, Maybe.Nope, Maybe(fmtString), args: _*)

  /**
   * Used to convert a processing error into a parse error so that it
   * looks like the same as other parse errors to tests that search for the
   * "Parse Error" string.
   */
  def toParseError = new ParseError(schemaContext, dataContext, Maybe(this), Maybe.Nope)

  /**
   * Used to convert a processing error into a unparse error so that it
   * looks like the same as other unparse errors to tests that search for the
   * "Unparse Error" string.
   */
  def toUnparseError = new UnparseError(schemaContext, dataContext, Maybe(this), Maybe.Nope)

}
