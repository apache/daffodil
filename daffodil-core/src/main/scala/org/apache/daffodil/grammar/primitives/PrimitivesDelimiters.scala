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

import org.apache.daffodil.Implicits._;
import org.apache.daffodil.dsom._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.dfa.TextParser
import org.apache.daffodil.processors.parsers.DelimiterTextParser
import org.apache.daffodil.processors.parsers.DelimiterTextType
import org.apache.daffodil.processors.parsers.{ Parser => DaffodilParser }
import org.apache.daffodil.processors.unparsers.DelimiterTextUnparser
import org.apache.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }
import org.apache.daffodil.schema.annotation.props.gen.EscapeKind
import org.apache.daffodil.schema.annotation.props.gen.LengthKind

object INoWarn5 { ImplicitsSuppressUnusedImportWarning() }

abstract class Text(es: Term, e: Term, guard: Boolean) extends StringDelimBase(es, guard) {

  lazy val eName = e.toString()

  lazy val positionalInfo = {
    if (e.isSequenceChild) {
      e.nearestEnclosingSequence match {
        case Some(es) => {
          val pos = e.positionInNearestEnclosingSequence - 1
          if (es.hasPrefixSep) {
            if (e.hasPriorRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)
              "after " + prior.diagnosticDebugName + " and before " + eName
            } else "before " + eName
          } else if (es.hasInfixSep)
            if (e.hasPriorRequiredSiblings && e.hasLaterRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)

              "after " + prior.diagnosticDebugName + " and before " + eName
            } else if (e.hasPriorRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)
              "after " + prior.diagnosticDebugName + " and before " + eName
            } else if (e.hasLaterRequiredSiblings) {
              val later: Term = es.groupMembers(pos + 1)
              "before " + later.diagnosticDebugName
            } else { "" }
          else if (es.hasPostfixSep)
            if (e.hasPriorRequiredSiblings && e.hasLaterRequiredSiblings) {
              val later: Term = es.groupMembers(pos + 1)

              "after " + eName + " and before " + later.diagnosticDebugName
            } else if (e.hasPriorRequiredSiblings) {
              val prior: Term = es.groupMembers(pos - 1)
              "after " + prior.diagnosticDebugName + " and before " + eName
            } else if (e.hasLaterRequiredSiblings) {
              val later: Term = es.groupMembers(pos + 1)
              "before " + later.diagnosticDebugName
            } else { "" }
          else
            ""
        }
        case None => ""
      }
    } else {
      ""
    }
  }
}

// NOTE: LiteralNil still uses this as it can only be Static/Constant
//
abstract class StaticText(delim: String, e: Term, eb: Term, kindString: String, guard: Boolean = true)
  extends Text(e, eb, guard) {

  Assert.invariant(delim != "") // shouldn't be here at all in this case.

  lazy val textParser = new TextParser(e.termRuntimeData)
}

abstract class DelimiterText(e: Term, eb: Term, delimiterType: DelimiterTextType.Type, guard: Boolean = true)
  extends Text(e, eb, guard) {

  lazy val textParser = new TextParser(e.termRuntimeData)

  val isDelimited = e match {
    case elemB: ElementBase => elemB.lengthKind == LengthKind.Delimited
    case _ => false
  }

  override lazy val parser: DaffodilParser = new DelimiterTextParser(e.termRuntimeData, textParser, positionalInfo, delimiterType, isDelimited)
  override lazy val unparser: DaffodilUnparser = new DelimiterTextUnparser(e.termRuntimeData, delimiterType)
}

case class Initiator(e: Term) extends DelimiterText(e, e, DelimiterTextType.Initiator) {
  Assert.invariant(e.hasInitiator)
}

case class SequenceSeparator(s: SequenceTermBase) extends DelimiterText(s, s, DelimiterTextType.Separator, s.hasSeparator)

case class Terminator(e: Term) extends DelimiterText(e, e, DelimiterTextType.Terminator) {
  Assert.invariant(e.hasTerminator)
}

abstract class StringDelimBase(e: Term, guard: Boolean) extends Terminal(e, guard) {
  override def toString = "StringDelimBase[" + name + "]"

  def checkDelimiterDistinctness(
    escapeSchemeKind: EscapeKind,
    optPadChar: Option[String],
    optEscChar: Option[String], // Could be a DFDL expression
    optEscEscChar: Option[String], // Could be a DFDL expression
    optEscBlkStart: Option[String],
    optEscBlkEnd: Option[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    // TODO: DFDL-451 - After conversing with Mike B. about this, we're putting this on the backburner.
    // Leaving the code here, just commented out the entry point until we can decide what is the appropriate
    // behavior here.
    //
    //    escapeSchemeKind match {
    //      case EscapeSchemeKind.None =>
    //        checkDelimiterDistinctness_(optPadChar, None, None, None, None, terminatingMarkup, context)
    //      case EscapeSchemeKind.Character =>
    //        checkDelimiterDistinctness_(optPadChar, optEscChar, optEscEscChar, None, None, terminatingMarkup, context)
    //      case EscapeSchemeKind.Block =>
    //        checkDelimiterDistinctness_(optPadChar, None, optEscEscChar,
    //          optEscBlkStart, optEscBlkEnd, terminatingMarkup, context)
    //    }
  }
}
