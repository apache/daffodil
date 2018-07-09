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

import org.apache.daffodil.grammar._
import org.apache.daffodil.dsom._
import org.apache.daffodil.processors.unparsers.SequenceChildUnparser
import org.apache.daffodil.processors.parsers.SequenceChildParser
import org.apache.daffodil.processors.parsers.ScalarOrderedRequiredUnseparatedSequenceChildParser
import org.apache.daffodil.processors.parsers.RepOrderedExactlyTotalOccursCountSeparatedSequenceChildParser
import org.apache.daffodil.processors.parsers.ScalarOrderedRequiredSeparatedSequenceChildParser
import org.apache.daffodil.processors.parsers.RepOrderedWithMinMaxUnseparatedSequenceChildParser
import org.apache.daffodil.processors.parsers.RepOrderedWithMinMaxSeparatedSequenceChildParser
import org.apache.daffodil.processors.parsers.RepOrderedExactlyNUnseparatedSequenceChildParser
import org.apache.daffodil.processors.parsers.RepOrderedExactlyTotalOccursCountUnseparatedSequenceChildParser
import org.apache.daffodil.processors.unparsers.RepOrderedExactlyNUnseparatedSequenceChildUnparser
import org.apache.daffodil.processors.unparsers.RepOrderedWithMinMaxUnseparatedSequenceChildUnparser
import org.apache.daffodil.processors.unparsers.ScalarOrderedRequiredUnseparatedSequenceChildUnparser
import org.apache.daffodil.processors.unparsers.RepOrderedWithMinMaxSeparatedSequenceChildUnparser
import org.apache.daffodil.processors.unparsers.ScalarOrderedRequiredSeparatedSequenceChildUnparser
import org.apache.daffodil.processors.unparsers.RepOrderedExactlyTotalOccursCountSeparatedSequenceChildUnparser
import org.apache.daffodil.processors.unparsers.RepOrderedExactlyTotalOccursCountUnseparatedSequenceChildUnparser
import org.apache.daffodil.processors.parsers.RepOrderedExactlyNSeparatedSequenceChildParser
import org.apache.daffodil.processors.unparsers.RepOrderedExactlyNSeparatedSequenceChildUnparser

abstract class SequenceChild(sq: SequenceTermBase, child: Term, groupIndex: Int)
  extends Terminal(child, true) {

  protected def childParser = child.termContentBody.parser
  protected def childUnparser = child.termContentBody.unparser

  final override def parser = sequenceChildParser
  final override def unparser = sequenceChildUnparser

  protected def sequenceChildParser: SequenceChildParser
  protected def sequenceChildUnparser: SequenceChildUnparser

  def optSequenceChildParser: Option[SequenceChildParser] =
    if (childParser.isEmpty) None else Some(parser)

  def optSequenceChildUnparser: Option[SequenceChildUnparser] =
    if (childUnparser.isEmpty) None else Some(unparser)

  protected lazy val sepGram = sq.sequenceSeparator
  protected lazy val sepParser = sepGram.parser
  protected lazy val sepUnparser = sepGram.unparser

  lazy val srd = sq.sequenceRuntimeData
  lazy val trd = child.termRuntimeData
}

class ScalarOrderedRequiredSequenceChild(sq: SequenceTermBase, term: Term, groupIndex: Int)
  extends SequenceChild(sq, term, groupIndex) {

  def sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true => new ScalarOrderedRequiredSeparatedSequenceChildParser(
      childParser, srd, trd, sepParser, sq.separatorPosition, sq.separatorSuppressionPolicy)
    case false => new ScalarOrderedRequiredUnseparatedSequenceChildParser(childParser, srd, trd)
  }
  def sequenceChildUnparser: SequenceChildUnparser = sq.hasSeparator match {
    case true => new ScalarOrderedRequiredSeparatedSequenceChildUnparser(
      childUnparser, srd, trd, sepUnparser, sq.separatorPosition, sq.separatorSuppressionPolicy)
    case false => new ScalarOrderedRequiredUnseparatedSequenceChildUnparser(childUnparser, srd, trd)
  }
}

sealed abstract class ElementSequenceChild(sq: SequenceTermBase, e: ElementBase, groupIndex: Int)
  extends SequenceChild(sq, e, groupIndex) {

  protected lazy val erd = e.elementRuntimeData
}

class RepOrderedExactlyNSequenceChild(sq: SequenceTermBase, e: ElementBase, groupIndex: Int, repeatCount: Long)
  extends ElementSequenceChild(sq, e, groupIndex) {

  def sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true => new RepOrderedExactlyNSeparatedSequenceChildParser(
      childParser, srd, erd, repeatCount, sepParser, sq.separatorPosition, sq.separatorSuppressionPolicy)
    case false => new RepOrderedExactlyNUnseparatedSequenceChildParser(childParser, srd, erd, repeatCount)
  }
  def sequenceChildUnparser: SequenceChildUnparser = sq.hasSeparator match {
    case true => new RepOrderedExactlyNSeparatedSequenceChildUnparser(
      childUnparser, srd, erd, repeatCount, sepUnparser, sq.separatorPosition, sq.separatorSuppressionPolicy)
    case false => new RepOrderedExactlyNUnseparatedSequenceChildUnparser(childUnparser, srd, erd, repeatCount)
  }
}

class RepOrderedExactlyTotalOccursCountSequenceChild(sq: SequenceTermBase, e: ElementBase, groupIndex: Int)
  extends ElementSequenceChild(sq, e, groupIndex) {

  protected lazy val ocGram = OccursCountExpression(e)
  protected lazy val ocParser = ocGram.parser
  protected lazy val ocUnparser = ocGram.unparser

  def sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true => new RepOrderedExactlyTotalOccursCountSeparatedSequenceChildParser(
      childParser, ocParser, srd, erd, sepParser, sq.separatorPosition, sq.separatorSuppressionPolicy)
    case false => new RepOrderedExactlyTotalOccursCountUnseparatedSequenceChildParser(childParser, ocParser, srd, erd)
  }
  def sequenceChildUnparser: SequenceChildUnparser = sq.hasSeparator match {
    case true => new RepOrderedExactlyTotalOccursCountSeparatedSequenceChildUnparser(
      childUnparser, srd, erd, sepUnparser, sq.separatorPosition, sq.separatorSuppressionPolicy)
    case false => new RepOrderedExactlyTotalOccursCountUnseparatedSequenceChildUnparser(childUnparser, srd, erd)
  }
}

class RepOrderedWithMinMaxSequenceChild(sq: SequenceTermBase, e: ElementBase, groupIndex: Int, baseName: String, min: Int = -1, max: Int = -1)
  extends ElementSequenceChild(sq, e, groupIndex) {

  def sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true => new RepOrderedWithMinMaxSeparatedSequenceChildParser(
      childParser, srd, erd, baseName, sepParser, sq.separatorPosition, sq.separatorSuppressionPolicy, min, max)
    case false => new RepOrderedWithMinMaxUnseparatedSequenceChildParser(childParser, srd, erd, baseName, min, max)
  }

  def sequenceChildUnparser: SequenceChildUnparser = sq.hasSeparator match {
    case true => new RepOrderedWithMinMaxSeparatedSequenceChildUnparser(
      childUnparser, srd, erd, baseName, sepUnparser, sq.separatorPosition, sq.separatorSuppressionPolicy, min, max)
    case false => new RepOrderedWithMinMaxUnseparatedSequenceChildUnparser(childUnparser, srd, erd, baseName, min, max)
  }
}
