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
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.dsom._
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.processors.unparsers._
import org.apache.daffodil.util.Misc
import org.apache.daffodil.runtime2.generators.CodeGeneratorState

/**
 * Base class for all kinds of sequences.
 */
abstract class SequenceCombinator(sq: SequenceTermBase, sequenceChildren: Seq[SequenceChild])
  extends Terminal(sq, sequenceChildren.length > 0) {

  /**
   * Shorthand for getting the sequence runtime data.
   */
  val srd = sq.sequenceRuntimeData

  override def toString() =
    "<" + Misc.getNameFromClass(this) + ">" +
      sequenceChildren.map { _.toString() }.mkString +
      "</" + Misc.getNameFromClass(this) + ">"
}

class OrderedSequence(sq: SequenceTermBase, sequenceChildrenArg: Seq[SequenceChild])
  extends SequenceCombinator(sq, sequenceChildrenArg) {

  private lazy val sepGram = sq.sequenceSeparator
  private lazy val sepParser = sepGram.parser
  private lazy val sepUnparser = sepGram.unparser

  private lazy val sequenceChildren = sequenceChildrenArg.toVector

  override lazy val parser: Parser = sq.hasSeparator match {
    case true => new OrderedSeparatedSequenceParser(
      srd, sq.separatorPosition, sepParser,
      sequenceChildren.flatMap { _.optSequenceChildParser })
    case false =>
      new OrderedUnseparatedSequenceParser(
        srd,
        sequenceChildren.flatMap { _.optSequenceChildParser })
  }
  override lazy val unparser: Unparser = {
    sq match {
      case sgr: SequenceGroupRef if sgr.isHidden =>
        /*
         * This is a requirement for all descendants of a hidden sequence, so we
         * SDE if all descendants are not fully defaultable, optional or OVC.
         */
        val nonUnparseableIfHidden = sq.groupMembers.filter(!_.canUnparseIfHidden)
        if (nonUnparseableIfHidden.nonEmpty) {
          SDE("Element(s) of hidden group must define dfdl:outputValueCalc, be defaultable or be optional:\n%s",
            nonUnparseableIfHidden.mkString("\n"))
        }
      case _ => {
        //do nothing as only GroupRefs have the concept of hiddenness at compile time
      }
    }
    val childUnparsers = sequenceChildren.flatMap { _.optSequenceChildUnparser }
    if (childUnparsers.isEmpty) new NadaUnparser(null)
    else {
      sq.hasSeparator match {
        case true => new OrderedSeparatedSequenceUnparser(
          srd,
          sq.separatorSuppressionPolicy, sq.separatorPosition, sepUnparser,
          childUnparsers)
        case false =>
          new OrderedUnseparatedSequenceUnparser(
            srd,
            childUnparsers)
      }
    }
  }

  override def generateCode(cgState: CodeGeneratorState): Unit = {
    //
    // To lift this draconian restriction, we have to
    // generate code for each of the children, and combine them into a block
    //
    sq.schemaDefinitionUnless(sequenceChildren.length == 1, "Only a single child of a sequence is supported.")
    val child1 = sequenceChildren(0)
    child1.generateCode(cgState)
  }
}

class UnorderedSequence(sq: SequenceTermBase, sequenceChildrenArg: Seq[SequenceChild], alternatives: Seq[Gram])
  extends SequenceCombinator(sq, sequenceChildrenArg) {

  import SeparatedSequenceChildBehavior._

  private lazy val sepGram = sq.sequenceSeparator
  private lazy val sepParser = sepGram.parser
  private lazy val sepUnparser = sepGram.unparser

  private lazy val sequenceChildren = sequenceChildrenArg.toVector

  private lazy val parsers = alternatives.map(_.parser)

  override lazy val parser: Parser = {

    lazy val choiceParser = new ChoiceParser(srd, parsers.toVector)

    sq.hasSeparator match {
      case true => {
        lazy val groupHelper = new NonPositionalGroupSeparatedSequenceChildParseResultHelper(
          srd,
          NonPositional,
          true, // Due to the nature of UOSeqs, could potentially be empty
          false) // and does not have required syntax

        lazy val groupParser = new GroupSeparatedUnorderedSequenceChildParser(
          choiceParser,
          srd,
          srd, // Won't actually be used
          sepParser,
          sq.separatorPosition,
          groupHelper)

        new UnorderedSeparatedSequenceParser(
          srd, sq.separatorPosition, sepParser,
          Vector(groupParser))
      }
      case false => {
        lazy val groupHelper = new GroupUnseparatedSequenceChildParseResultHelper(
          srd,
          true, // Due to the nature of UOSeqs, could potentially be empty
          false) // and does not have required syntax

        lazy val groupParser = new ScalarUnorderedUnseparatedSequenceChildParser(
          choiceParser,
          srd,
          srd, // Won't actually be used
          groupHelper)

        new UnorderedUnseparatedSequenceParser(
          srd,
          Vector(groupParser))
      }
    }
  }

  override lazy val unparser: Unparser = {
    sq match {
      case sgr: SequenceGroupRef if sgr.isHidden =>
        /*
         * This is a requirement for all descendants of a hidden sequence, so we
         * SDE if all descendants are not fully defaultable, optional or OVC.
         */
        val nonUnparseableIfHidden  = sq.groupMembers.filter( !_.canUnparseIfHidden)
        if (nonUnparseableIfHidden.nonEmpty) {
          SDE("Element(s) of hidden group must define dfdl:outputValueCalc, be defaultable or be optional:\n%s",
            nonUnparseableIfHidden.mkString("\n"))
        }
      case _ => {
        //do nothing as only GroupRefs have the concept of hiddenness at compile state
      }
    }
    val childUnparsers = sequenceChildren.flatMap { _.optSequenceChildUnparser }
    if (childUnparsers.isEmpty) new NadaUnparser(null)
    else {
      sq.hasSeparator match {
        case true => new OrderedSeparatedSequenceUnparser(
          srd,
          SeparatorSuppressionPolicy.AnyEmpty, sq.separatorPosition, sepUnparser,
          childUnparsers)
        case false =>
          new OrderedUnseparatedSequenceUnparser(
            srd,
            childUnparsers)
      }
    }
  }
}
