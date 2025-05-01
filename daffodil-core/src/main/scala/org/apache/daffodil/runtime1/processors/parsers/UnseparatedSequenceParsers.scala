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
package org.apache.daffodil.runtime1.processors.parsers

import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.OccursCountEv
import org.apache.daffodil.runtime1.processors.Processor
import org.apache.daffodil.runtime1.processors.{ SequenceRuntimeData, TermRuntimeData }

trait Unseparated { self: SequenceChildParser =>

  def parseResultHelper: UnseparatedSequenceChildParseResultHelper

  final def parseOne(
    pstate: PState,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    val prevBitPosBeforeChild = pstate.bitPos0b
    self.childParser.parse1(pstate)
    val res = parseResultHelper.computeParseAttemptStatus(
      self,
      prevBitPosBeforeChild,
      pstate,
      requiredOptional
    )
    res
  }

  final def isPositional = true
}

class ScalarOrderedUnseparatedSequenceChildParser(
  override val childParser: Parser,
  override val srd: SequenceRuntimeData,
  override val trd: TermRuntimeData,
  override val parseResultHelper: UnseparatedSequenceChildParseResultHelper
) extends SequenceChildParser(childParser, srd, trd)
  with Unseparated
  with NonRepeatingSequenceChildParser

class RepOrderedExactlyNUnseparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  override val parseResultHelper: UnseparatedSequenceChildParseResultHelper,
  val repeatCount: Long
) extends OccursCountExactParser(childParser, srd, erd)
  with Unseparated

class RepOrderedExpressionOccursCountUnseparatedSequenceChildParser(
  childParser: Parser,
  ocEv: OccursCountEv,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  override val parseResultHelper: UnseparatedSequenceChildParseResultHelper
) extends OccursCountExpressionParser(childParser, srd, erd, ocEv)
  with Unseparated

class RepOrderedWithMinMaxUnseparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  override val parseResultHelper: UnseparatedSequenceChildParseResultHelper
) extends OccursCountMinMaxParser(childParser, srd, erd)
  with Unseparated

class OrderedUnseparatedSequenceParser(
  rd: SequenceRuntimeData,
  override val childParsers: Array[SequenceChildParser]
) extends SequenceParserBase(rd, isOrdered = true) {

  override def runtimeDependencies = Vector()

  override def childProcessors: Vector[Processor] = childParsers.toVector
}

class UnorderedUnseparatedSequenceParser(
  rd: SequenceRuntimeData,
  override val childParsers: Array[SequenceChildParser]
) extends SequenceParserBase(rd, isOrdered = false) {
  override def runtimeDependencies = Vector()

  override def childProcessors: Vector[Processor] = childParsers.toVector
}
