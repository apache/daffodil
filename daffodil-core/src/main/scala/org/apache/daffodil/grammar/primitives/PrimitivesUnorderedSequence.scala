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

import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.dsom.Term
import org.apache.daffodil.dsom.SequenceTermBase
import org.apache.daffodil.grammar.UnaryGram
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.processors.unparsers.Unparser

object UnorderedSequence {
  def apply(context: Term, eGram: => Gram) = {
    // mandatory little optimization here. If there are no statements (most common case), then let's
    // shortcut and just use the guts parser.

    Assert.usageErrorUnless(context.isInstanceOf[SequenceTermBase], "The context passed to UnorderedSequence must be a Sequence.")

    val ctxt = context.asInstanceOf[SequenceTermBase]

    new UnorderedSequence(ctxt, eGram)
  }
}

class UnorderedSequence private (context: SequenceTermBase, eGramArg: => Gram) // private to force use of the object as factory
  extends UnaryGram(context, eGramArg) {

  lazy val eGram = eGramArg // once only

  // Forced as part of required evaluations in Sequence
  //context.checkIfValidUnorderedSequence

  val uoSeqParser = eGram.parser
  val sortOrder = {
    val members = context.groupMembers.map(t => {
      t match {
        case s: SequenceTermBase => s.groupMembers
        case _ => Seq(t)
      }
    }).flatten

    members.map(t => {
      val erd = t.runtimeData.asInstanceOf[ElementRuntimeData]
      val name = erd.name
      val ns = erd.targetNamespace
      (name, ns)
    })
  }

  val scalarMembers =
    context.groupMembers.filter(t => t.isInstanceOf[ElementBase]).filter {
      case eb: ElementBase => eb.isScalar
    }.map {
      case eb: ElementBase => {
        val erd = eb.runtimeData.asInstanceOf[ElementRuntimeData]
        (erd.name, erd.path, erd.targetNamespace)
      }
    }

  lazy val parser: Parser = ??? // new UnorderedSequenceParser(context.modelGroupRuntimeData, sortOrder, scalarMembers, uoSeqParser)
  lazy val unparser: Unparser = ???

}
