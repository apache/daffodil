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
package org.apache.daffodil.processors.parsers

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors._
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.schema.annotation.props.gen.SeparatorPosition
import ArrayIndexStatus._
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.infoset.DIElement
import org.apache.daffodil.infoset.DIComplex
import org.apache.daffodil.schema.annotation.props.gen.LengthKind

trait Separated { self: SequenceChildParser =>

  def sep: Parser
  def spos: SeparatorPosition
  def trd: TermRuntimeData
  def parseResultHelper: SeparatedSequenceChildParseResultHelper

  import SeparatorPosition._

  protected final val separatorHelper = spos match {
    case Prefix => new PrefixSeparatorHelper(sep, childParser, this)
    case Infix => new InfixSeparatorHelper(sep, childParser, this)
    case Postfix => new PostfixSeparatorHelper(sep, childParser, this,
      parseResultHelper.isSimpleDelimited)
  }

  final def isPositional: Boolean =
    parseResultHelper.separatedSequenceChildBehavior.isInstanceOf[SeparatedSequenceChildBehavior.PositionalLike]

  final def parseOne(pstate: PState, requiredOptional: RequiredOptionalStatus): ParseAttemptStatus = {
    separatorHelper.parseOneWithSeparator(pstate, requiredOptional)
  }

  final override def finalChecks(pstate: PState, resultOfTry: ParseAttemptStatus, priorResultOfTry: ParseAttemptStatus): Unit =
    parseResultHelper.finalChecks(self, pstate, resultOfTry, priorResultOfTry)

}

sealed abstract class ScalarOrderedSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData,
  override val sep: Parser,
  override val spos: SeparatorPosition,
  override val parseResultHelper: SeparatedSequenceChildParseResultHelper)
  extends SequenceChildParser(childParser, srd, trd)
  with Separated
  with NonRepeatingSequenceChildParser

final class ScalarOrderedElementSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData,
  sep: Parser,
  spos: SeparatorPosition,
  prh: SeparatedSequenceChildParseResultHelper)
  extends ScalarOrderedSeparatedSequenceChildParser(childParser, srd, trd, sep, spos, prh)

final class GroupSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  val mrd: ModelGroupRuntimeData,
  sep: Parser,
  spos: SeparatorPosition,
  prh: SeparatedSequenceChildParseResultHelper)
  extends ScalarOrderedSeparatedSequenceChildParser(childParser, srd, mrd, sep, spos, prh)

final class RepOrderedExactlyNSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  override val sep: Parser,
  override val spos: SeparatorPosition,
  override val parseResultHelper: SeparatedSequenceChildParseResultHelper)
  extends OccursCountExactParser(childParser, srd, erd)
  with Separated

final class RepOrderedExactlyTotalOccursCountSeparatedSequenceChildParser(
  childParser: Parser,
  ocEv: OccursCountEv,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  override val sep: Parser,
  override val spos: SeparatorPosition,
  override val parseResultHelper: SeparatedSequenceChildParseResultHelper)
  extends OccursCountExpressionParser(childParser, srd, erd, ocEv)
  with Separated

final class RepOrderedWithMinMaxSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  override val sep: Parser,
  override val spos: SeparatorPosition,
  override val parseResultHelper: SeparatedSequenceChildParseResultHelper)
  extends OccursCountMinMaxParser(childParser, srd, erd)
  with Separated

final class OrderedSeparatedSequenceParser(
  rd: SequenceRuntimeData,
  spos: SeparatorPosition,
  sep: Parser,
  childrenArg: Vector[SequenceChildParser])
  extends OrderedSequenceParserBase(rd, childrenArg) {

  override lazy val childProcessors = (sep +: childrenArg.asInstanceOf[Seq[Parser]]).toVector
}
