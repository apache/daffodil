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

package org.apache.daffodil.runtime1.processors.unparsers
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.iapi._
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.runtime1.processors._

class UnparseAlternativeFailed(
  rd: TermRuntimeData,
  loc: DataLocation,
  val errors: Seq[Diagnostic]
) extends UnparseError(
    One(rd.schemaFileLocation),
    One(loc),
    Maybe.Nope,
    Maybe("Alternative failed. Reason(s): %s"),
    errors
  )

object UnparseError {
  def apply(
    rd: Maybe[SchemaFileLocation],
    loc: Maybe[DataLocation],
    formatString: String,
    args: Any*
  ) = {
    val ue = new UnparseError(rd, loc, Maybe.Nope, Maybe(formatString), args: _*)
    ue.toss
  }
}

class UnparseError(
  rd: Maybe[SchemaFileLocation],
  loc: Maybe[DataLocation],
  causedBy: Maybe[Throwable],
  kind: Maybe[String],
  args: Any*
) extends ProcessingError("Unparse", rd, loc, causedBy, kind, args: _*) {
  def this(rd: Maybe[SchemaFileLocation], loc: Maybe[DataLocation], causedBy: Throwable) =
    this(rd, loc, Maybe(causedBy), Maybe.Nope)
}
