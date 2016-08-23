/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.grammar

import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.compiler.ParserOrUnparser
import edu.illinois.ncsa.daffodil.processors.NadaParser
import edu.illinois.ncsa.daffodil.processors.NadaUnparser
import edu.illinois.ncsa.daffodil.compiler.ForUnparser
import edu.illinois.ncsa.daffodil.compiler.ForParser
import edu.illinois.ncsa.daffodil.util.LogLevel

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
final class Prod(nameArg: String, val sc: SchemaComponent, guard: Boolean, gramArg: => Gram, override val forWhat: ParserOrUnparser)
  extends NamedGram(sc) {

  final override def deref = gram

  final override def name = nameArg

  final override lazy val path = sc.path + "@@Prod(" + prettyName + ")"

  final override lazy val gram: Gram = {
    guard match {
      case true => {
        val g = gramArg // exactly once.
        g match {
          case p: Prod => {
            p.gram // recursively force this
          }
          case _ => //ok
        }
        g
      }
      case false => {
        log(LogLevel.Debug, "Prod %s removed.", name)
        EmptyGram
      }
    }
  }

  final override def isEmpty = if (!guard) true else gram.isEmpty

  final override lazy val parser = {
    (forWhat, gram.forWhat) match {
      case (ForUnparser, _) => new NadaParser(context.runtimeData) // TODO: detect this and remove from final parser
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
          case (ForParser, _) => new NadaUnparser(context.runtimeData) // TODO: detect this and remove from final unparser
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
