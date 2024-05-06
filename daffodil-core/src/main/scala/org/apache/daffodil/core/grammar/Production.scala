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

package org.apache.daffodil.core.grammar

import org.apache.daffodil.core.compiler.ForParser
import org.apache.daffodil.core.compiler.ForUnparser
import org.apache.daffodil.core.compiler.ParserOrUnparser
import org.apache.daffodil.core.dsom.SchemaComponent
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.processors.parsers.NadaParser
import org.apache.daffodil.unparsers.runtime1.NadaUnparser

/**
 * Prod or Grammar Production
 *
 * Note the call by name on the GramArg. We don't evaluate the GramArg at all unless the guard is true.
 *
 * Guards are used so we can have grammars that include all possibilities,
 * but where examining the format properties specifically would indicate that some of those
 * possibilities are precluded. The guard causes that term to just splice itself out
 * of the grammar.
 *
 * Note that it is crucial that the guardArg is passed by value, and the gramArg is
 * passed by name.
 *
 * Prod objects are not required. They essentially provide some useful debug capability
 * because a grammar term object will display as it's name, not as some anonymous object.
 */
final class Prod(
  nameArg: String,
  val sc: SchemaComponent,
  val guard: Boolean,
  gramArg: => Gram,
  override val forWhat: ParserOrUnparser
) extends NamedGram(sc) {

  final override lazy val deref = gram

  final override lazy val name = nameArg

  override def toString() = "<" + name + ">" + gram.toString + "</" + name + ">"

  final override lazy val path = sc.path + "@@Prod(" + diagnosticDebugName + ")"

  private lazy val g = gramArg // once only

  final override lazy val gram: Gram = {
    guard match {
      case true => {
        g match {
          case p: Prod => {
            p.gram // recursively force this
          }
          case _ => // ok
        }
        g
      }
      case false => {
        Logger.log.debug(s"Prod ${name} removed.")
        EmptyGram
      }
    }
  }

  final override lazy val isEmpty = if (!guard) true else gram.isEmpty

  final override lazy val parser = {
    (forWhat, gram.forWhat) match {
      case (ForUnparser, _) =>
        new NadaParser(context.runtimeData) // TODO: detect this and remove from final parser
      case (_, ForUnparser) => new NadaParser(gram.context.runtimeData)
      case _ => gram.parser
    }
  }

  final override lazy val unparser = {
    val unp =
      if (gram.isEmpty) {
        EmptyGram.unparser
      } else {
        (forWhat, gram.forWhat) match {
          case (ForParser, _) =>
            new NadaUnparser(
              context.runtimeData
            ) // TODO: detect this and remove from final unparser
          case (_, ForParser) => new NadaUnparser(gram.context.runtimeData)
          case _ => gram.unparser
        }
      }
    if (unp.isEmpty)
      new NadaUnparser(context.runtimeData)
    else
      unp
  }
}
