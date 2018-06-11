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

abstract class SequenceCombinator(sq: SequenceTermBase, sequenceChildren: Seq[SequenceChild])
  extends Terminal(sq, sequenceChildren.length > 0) {
  val srd = sq.sequenceRuntimeData
}

sealed abstract class OrderedSequenceBase(sq: SequenceTermBase, sequenceChildren: Seq[SequenceChild])
  extends SequenceCombinator(sq, sequenceChildren) {

  override def toString() =
    "<" + Misc.getNameFromClass(this) + ">" +
      sequenceChildren.map { _.toString() }.mkString +
      "</" + Misc.getNameFromClass(this) + ">"
}

class OrderedSequence(sq: SequenceTermBase, sequenceChildren: Seq[SequenceChild])
  extends OrderedSequenceBase(sq, sequenceChildren) {

  private lazy val sepGram = sq.sequenceSeparator
  private lazy val sepParser = sepGram.parser
  private lazy val sepUnparser = sepGram.unparser

  override lazy val parser: Parser = sq.hasSeparator match {
    case true => new OrderedSeparatedSequenceParser(srd,
      sq.separatorSuppressionPolicy, sq.separatorPosition, sepParser,
      sequenceChildren.flatMap { _.optSequenceChildParser })
    case false =>
      new OrderedUnseparatedSequenceParser(srd,
        sequenceChildren.flatMap { _.optSequenceChildParser })
  }
  override lazy val unparser: Unparser = {
    sq.checkHiddenSequenceIsDefaultableOrOVC
    val childUnparsers = sequenceChildren.flatMap { _.optSequenceChildUnparser }
    if (childUnparsers.isEmpty) new NadaUnparser(null)
    else
      sq.hasSeparator match {
        case true => new OrderedSeparatedSequenceUnparser(srd,
          sq.separatorSuppressionPolicy, sq.separatorPosition, sepUnparser,
          childUnparsers)
        case false =>
          new OrderedUnseparatedSequenceUnparser(srd,
            childUnparsers)
      }
  }
}
