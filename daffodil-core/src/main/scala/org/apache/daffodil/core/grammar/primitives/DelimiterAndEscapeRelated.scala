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

package org.apache.daffodil.core.grammar.primitives

import org.apache.daffodil.core.dsom._
import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.core.grammar.Terminal
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.processors.parsers._
import org.apache.daffodil.runtime1.processors.parsers.{ Parser => DaffodilParser }
import org.apache.daffodil.runtime1.processors.unparsers.{ Unparser => DaffodilUnparser }
import org.apache.daffodil.unparsers.runtime1._

case class DelimiterStackCombinatorSequence(sq: SequenceTermBase, body: Gram)
  extends Terminal(sq, !body.isEmpty) {
  lazy val pInit =
    if (sq.initiatorParseEv.isConstantEmptyString) Nope else One(sq.initiatorParseEv)
  lazy val pSep =
    if (sq.hasSeparator && !sq.separatorParseEv.isConstantEmptyString) One(sq.separatorParseEv)
    else Nope
  lazy val pTerm =
    if (sq.terminatorParseEv.isConstantEmptyString) Nope else One(sq.terminatorParseEv)

  lazy val uInit =
    if (sq.initiatorParseEv.isConstantEmptyString) Nope else One(sq.initiatorUnparseEv)
  lazy val uSep =
    if (sq.hasSeparator && !sq.separatorParseEv.isConstantEmptyString)
      One(sq.separatorUnparseEv)
    else Nope
  lazy val uTerm =
    if (sq.terminatorParseEv.isConstantEmptyString) Nope else One(sq.terminatorUnparseEv)

  lazy val parser: DaffodilParser = new DelimiterStackParser(
    (pInit.toList ++ pSep.toList ++ pTerm.toList).toArray,
    sq.runtimeData,
    body.parser
  )

  override lazy val unparser: DaffodilUnparser =
    new DelimiterStackUnparser(uInit, uSep, uTerm, sq.termRuntimeData, body.unparser)
}

case class DelimiterStackCombinatorChoice(ch: ChoiceTermBase, body: Gram)
  extends Terminal(ch, !body.isEmpty) {
  lazy val pInit =
    if (ch.initiatorParseEv.isConstantEmptyString) Nope else One(ch.initiatorParseEv)
  lazy val pTerm =
    if (ch.terminatorParseEv.isConstantEmptyString) Nope else One(ch.terminatorParseEv)

  lazy val uInit =
    if (ch.initiatorParseEv.isConstantEmptyString) Nope else One(ch.initiatorUnparseEv)
  lazy val uTerm =
    if (ch.terminatorParseEv.isConstantEmptyString) Nope else One(ch.terminatorUnparseEv)

  lazy val parser: DaffodilParser = new DelimiterStackParser(
    (pInit.toList ++ pTerm.toList).toArray,
    ch.runtimeData,
    body.parser
  )

  override lazy val unparser: DaffodilUnparser =
    new DelimiterStackUnparser(uInit, None, uTerm, ch.termRuntimeData, body.unparser)
}

case class DelimiterStackCombinatorElement(e: ElementBase, body: Gram)
  extends Terminal(e, !body.isEmpty) {
  lazy val pInit =
    if (e.initiatorParseEv.isConstantEmptyString) Nope else One(e.initiatorParseEv)
  lazy val pTerm =
    if (e.terminatorParseEv.isConstantEmptyString) Nope else One(e.terminatorParseEv)

  lazy val uInit =
    if (e.initiatorParseEv.isConstantEmptyString) Nope else One(e.initiatorUnparseEv)
  lazy val uTerm =
    if (e.terminatorParseEv.isConstantEmptyString) Nope else One(e.terminatorUnparseEv)

  lazy val delims = (pInit.toList ++ pTerm.toList)

  override def toString() = {
    val delimAttrib =
      delims.map { _.toString }.map { XMLUtils.escape(_).toString() }.mkString(" ")
    "<" + Misc.getNameFromClass(this) + " delims='" + delimAttrib + "'>" +
      body.toString() +
      "</" + Misc.getNameFromClass(this) + ">"
  }
  lazy val parser: DaffodilParser = {
    val p = body.parser
    if (p.isEmpty) p
    else new DelimiterStackParser(delims.toArray, e.termRuntimeData, p)
  }

  override lazy val unparser: DaffodilUnparser = {
    val u = body.unparser
    if (u.isEmpty) u
    else new DelimiterStackUnparser(uInit, None, uTerm, e.termRuntimeData, u)
  }
}

case class DynamicEscapeSchemeCombinatorElement(e: ElementBase, body: Gram)
  extends Terminal(e, !body.isEmpty) {

  val schemeParseOpt = e.optionEscapeScheme.map { _.escapeSchemeParseEv }
  val schemeUnparseOpt = e.optionEscapeScheme.map { _.escapeSchemeUnparseEv }

  val schemeParseIsConstant = schemeParseOpt.map(_.isConstant).getOrElse(true)
  val schemeUnparseIsConstant = schemeUnparseOpt.map(_.isConstant).getOrElse(true)

  Assert.invariant(!schemeParseIsConstant || !schemeUnparseIsConstant)

  lazy val parser: DaffodilParser = {
    val p = body.parser
    if (p.isEmpty || schemeParseIsConstant) p
    else new DynamicEscapeSchemeParser(schemeParseOpt.get, e.termRuntimeData, p)
  }

  override lazy val unparser: DaffodilUnparser = {
    val u = body.unparser
    if (u.isEmpty || schemeUnparseIsConstant) u
    else new DynamicEscapeSchemeUnparser(schemeUnparseOpt.get, e.termRuntimeData, u)
  }
}
