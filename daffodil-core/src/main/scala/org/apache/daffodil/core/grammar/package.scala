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

package org.apache.daffodil.core

/**
 * Grammar Mixins for Schema Compilation
 *
 * DFDL is defined using a data syntax grammar. Daffodil compiles a DFDL schema
 * into runtime data structures using a technique that tries to be faithful to
 * this notion of a data syntax grammar. The exact data syntax grammar used
 * in the DFDL specification is not really suitable to base an implementation on,
 * but where possible we have named the grammar production rules, and terminals of
 * the grammar consistently with the DFDL specification.
 *
 * The grammar rules are members of the DSOM traits/classes that use a
 * grammar notation, organized as grammar rules the applicability of which
 * is controlled by boolean test guard expressions.
 *
 * In addition to the grammar rules, these mixins contain the methods that analzye
 * the DFDL schema with its DFDL annotations, to determine whether the grammar
 * rule is applicable. Most optimizations in the Daffodil schema compiler are
 * implemented by way of a grammar rule or [[Terminal]] which is included or
 * excluded (aka guarded) by an optimization.
 *
 * The grammar rule mixins all inherit from the [[Gram]] trait which provides
 * operators for expressing the rules. The concrete classes are the
 * terminals of the grammar (instances of [[Terminal]]) and these
 * are either primitives or combinators. The primitves and combinators are
 * defined in the [[org.apache.daffodil.core.grammar.primitives]] package.
 *
 * This all works very much like Scala's `scala.util.parsing.combinator` library,
 * which is described in the [[http://www.artima.com/pins1ed/ Programming in Scala]] book
 * in the [[http://www.artima.com/pins1ed/combinator-parsing.html chapter on Combinator Parsing]].
 * However, Daffodil's grammar adds the notion of rich predicate guards controlling
 * the rules, and of course the result of the grammar is an entirely different
 * data structure. But but the way rules are expressed and use of operators
 * like "`~`" and "`||`" to create a little grammar composition language are similar.
 *
 * It is best to illustrate how the grammar works by an example drawn from model groups.
 * This example no longer matches the code of the actual implementation, but
 * illustrates the ideas behind the grammar:
 * @example
 * {{{
 * trait ModelGroupMixin extends ... {
 *
 * lazy val modelGroup = groupLeftFraming ~ groupContent ~ groupRightFraming
 *
 * lazy val groupLeftFraming = LeadingSkipRegion(this) ~ AlignmentFill(this)
 *
 * lazy val groupRightFraming = TrailingSkipRegion(this)
 * }
 * }}}
 * Non-terminals of the data syntax grammar are ordinary lazy val members named
 * beginning with lower case letter. Terminals of the grammar are classes
 * and so are named beginning with an upper case letter.
 *
 * A terminal like `LeadingSkipRegion` will optimize itself out by evaluating a
 * guard predicate test. Basically, if the DFDL schema has a `dfdl:leadingSkip`
 * property of '0', then the `LeadingSkipRegion` of the data syntax grammar is zero
 * width, so this terminal defines itself so that the `isEmpty` method returns
 * `true`. That enables the "`~`" operator to see that it has an empty grammar term
 * on it's left, hence, the "`~`" operator can reduce to just the right grammar term.
 *
 * The right grammar term, `AlignmentFill`, requires a much more sophisticated
 * analysis of the schema and properties, but ultimately could also decide that
 * it is not needed, and so `isEmpty`. Then the "`~`" operator will see that both sides are
 * empty and it itself is then empty. That allows the modelGroup rule to recognize that
 * for this DFDL schema there is no `groupLeftFraming`.
 *
 * The net result of this is all the grammar regions that are not applicable disappear
 * from the grammar. What is left is a nest of combinators and primitives suitable
 * for this specific DFDL schema's runtime to be generated.
 *
 * Lazy evaluation and by-name argument passing insure that this occurs without
 * evaluating any irrelevant grammar rules.
 *
 * At the end, the schema compiler generates a parser or unparser instance
 * by "asking" the now-optimized grammar data structure for a parser or unparser.
 * This means really by invoking the corresponding
 * method (`parser()` or `unparser()`) on the grammar object.
 * Recursively each combinator constructs an instance of
 * a class that implements
 * `org.apache.daffodil.runtime1.processors.parsers.Parser` (or
 * `org.apache.daffodil.runtime1.processors.unparsers.Unparser`)
 * and that instance recursively calls the parsers (or unparsers) generated
 * from the parts of the grammar which did not optimize themselves away.
 * Primitves construct atomic primitive parsers (and unparsers) for things like
 * delimiters, alignment regions, or simple type values.
 *
 * ==Futures==
 *
 * Many grammar rules are defined as instances of the [[Prod]] class using the `prod` method.
 * This is not strictly speaking needed (the example above doesn't use it), but is intended
 * to define named extension points in the grammar for a future DFDL extension
 * capability. The idea is that the named productions would be the places
 * where external extensions to the grammar would be examined and utilized to
 * augment the internal grammar rule definitions. These externally defined productions
 * could then incorporate into the grammar, new externally defined primitives,
 * or possibly even combinators, which ultimately generate calls to externally
 * defined parser (or unparser) instances.
 *
 * Currently the use of `prod` is ad-hoc and haphazard.
 */
package object grammar {
  // This object exists for scaladoc purposes.
}
