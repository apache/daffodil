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

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.dsom._
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.grammar.HasNoUnparser

abstract class Primitive(e: Term, guard: Boolean = false)
  extends Terminal(e, guard) {
  override def toString = "Prim[" + name + "]"
}

/**
 * For stubbing out primitives that are placeholders
 */
abstract class UnimplementedPrimitive(e: Term, guard: Boolean = false)
  extends Primitive(e, guard)
  with HasNoUnparser {
  override final lazy val parser = hasNoParser
}

// base stub classes

// case class NoValue(e: GlobalElementDecl, guard: Boolean = true) extends UnimplementedPrimitive(e, guard)

case class SaveInputStream(e: ElementBase, guard: Boolean = true) extends UnimplementedPrimitive(e, guard)

case class SetEmptyInputStream(e: ElementBase, guard: Boolean = true) extends UnimplementedPrimitive(e, guard)

case class RestoreInputStream(e: ElementBase, guard: Boolean = true) extends UnimplementedPrimitive(e, guard)

case class NotStopValue(e: ElementBase with LocalElementMixin) extends UnimplementedPrimitive(e, e.hasStopValue)

case class StopValue(e: ElementBase with LocalElementMixin) extends UnimplementedPrimitive(e, e.hasStopValue)

case class TheDefaultValue(e: ElementBase) extends UnimplementedPrimitive(e, e.isDefaultable)

case class UnicodeByteOrderMark(e: Root) extends UnimplementedPrimitive(e, false)
