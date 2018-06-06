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

import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.dsom._
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.processors.unparsers._
import org.apache.daffodil.util.Misc

sealed abstract class OrderedSequenceBase(sq: SequenceTermBase, rawTerms: Seq[Term])
  extends Terminal(sq, rawTerms.length > 0) {

  private lazy val terms = rawTerms.filterNot { _.termContentBody.isEmpty }

  // if all the body terms are empty, this causes this whole combinator to
  // optimize away.
  override final lazy val isEmpty = super.isEmpty || terms.isEmpty

  override def toString() =
    "<" + Misc.getNameFromClass(this) + ">" +
      rawTerms.map { _.toString() }.mkString +
      "</" + Misc.getNameFromClass(this) + ">"

  protected lazy val parserPairs = terms.flatMap { term =>
    val isNotRequired = !term.isRequired
    val gram = term match {
      case e: ElementBase if isNotRequired || term.isArray => e.recurrance
      case e: ElementBase => e.enclosedElement
      case _ => term.termContentBody
    }
    val p = gram.parser
    if (p.isEmpty) Nil
    else List((term.termRuntimeData, gram.parser))
  }.toVector

  protected lazy val unparserPairs = terms.flatMap { term =>
    val isNotRequired = !term.isRequired
    val gram = term match {
      case e: ElementBase if isNotRequired || term.isArray => e.recurrance
      case e: ElementBase => e.enclosedElement
      case _ => term.termContentBody
    }
    val u = gram.unparser
    if (u.isEmpty) Nil
    else List((term.termRuntimeData, gram.unparser))
  }.toVector

  protected lazy val parsers = parserPairs.map { _._2 }

  protected lazy val unparsers = unparserPairs.map { _._2 }
}

case class OrderedUnseparatedSequence(sq: SequenceTermBase, rawTerms: Seq[Term])
  extends OrderedSequenceBase(sq, rawTerms) {

  lazy val parser: Parser =
    new OrderedUnseparatedSequenceParser(sq.termRuntimeData, parsers)

  override lazy val unparser: Unparser = {
    sq.checkHiddenSequenceIsDefaultableOrOVC
    new OrderedUnseparatedSequenceUnparser(sq.modelGroupRuntimeData, unparsers)
  }
}

case class OrderedSeparatedSequence(sq: SequenceTermBase, rawTerms: Seq[Term])
  extends OrderedSequenceBase(sq, rawTerms) {

  lazy val parser: Parser = {
    new OrderedSeparatedSequenceParser(sq.termRuntimeData, sq.separatorSuppressionPolicy,
      sq.separatorPosition, sq.sequenceSeparator.parser, parserPairs)
  }

  override lazy val unparser: Unparser = {
    sq.checkHiddenSequenceIsDefaultableOrOVC
    new OrderedSeparatedSequenceUnparser(sq.modelGroupRuntimeData, sq.separatorPosition,
      sq.sequenceSeparator.unparser, unparserPairs)
  }
}
