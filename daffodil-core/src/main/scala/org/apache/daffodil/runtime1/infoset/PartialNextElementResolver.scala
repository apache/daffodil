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

package org.apache.daffodil.runtime1.infoset

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.ResettableIterator
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.lib.xml.QNameBase
import org.apache.daffodil.lib.xml.StepQName
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.NamespaceAmbiguousElementErrorERD
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.UnexpectedElementErrorERD

/**
 * Mixin for InfosetInputter
 */
trait NextElementResolver { self: InfosetInputter =>

  /**
   * Resolves the next element, or fails with an Unparse Error if there is
   * no mapping.
   */
  final def nextElement(
    name: String,
    nameSpace: String,
    hasNamespace: Boolean
  ): ElementRuntimeData = {
    Logger.log.debug(s"NextERDResovler -> trdStack: $trdStack")
    val iter = trdStack.iter
    iter.reset()
    var firstOne: Boolean = true
    var maybeERD: Maybe[ElementRuntimeData] = Nope
    var breakOut: Boolean = false

    while (iter.hasNext && maybeERD.isEmpty && !breakOut) {
      val trd = iter.next()
      trd match {
        case complexElementERD: ElementRuntimeData
            if (complexElementERD.isComplexType) && !firstOne => {
          //
          // We iterated down to a complex type.
          // That means we can't be expecting another child element
          // as we're expecting an end-of element event instead
          //
          maybeERD = error(iter, name, nameSpace, Some(complexElementERD))
          breakOut = true
        }
        case _ => // ok. Term is not a ERD for a complex type. Carry on.
      }
      if (!breakOut) {
        firstOne = false
        val resolver = trd.partialNextElementResolver
        //
        // This resolver will either resolve, pass on resolving (return Nope),
        // in which case we go around the loop and move down the stack,
        // or it will issue an UnparseError if there are required elements in the
        // set of possible elements.
        //
        Logger.log.debug(s"""
          NextERDResovler -> current trd: $trd\n
          NextERDResovler -> resolver: $resolver\n
          NextERDResovler -> name: $name\n""")
        maybeERD = resolver.maybeNextElement(name, nameSpace, hasNamespace)
        // The cases where ERD is in a hidden context are addressed where the elements
        // are assigned their value i.e ElementUnparser
      }
    } // end while
    //
    // We exited the loop, meaning either we have an ERD that is the resolution
    // or the stack of TRDs is empty.
    //
    // The latter is a boundary case that comes up
    // only in unit tests where we use a root element that is a simple type.
    // In that case, the loop above won't run into an ERD for a complex type element
    // and fail due to that, so we have to fail here.
    //
    if (maybeERD.isEmpty) {
      error(iter, name, nameSpace, None).get
    } else {
      maybeERD.get
    }
  }

  private val trdStack = new MStackOf[TermRuntimeData]

  final def pushTRD(trd: TermRuntimeData): Unit = {
    trdStack.push(trd)
  }

  final def maybeTopTRD(): Maybe[TermRuntimeData] = {
    if (!trdStack.isEmpty) One(trdStack.top)
    else Nope
  }

  final def popTRD(): TermRuntimeData = {
    val res = trdStack.pop
    res
  }

  private def error(
    iter: ResettableIterator[TermRuntimeData],
    name: String,
    namespace: String,
    optTRD: Option[TermRuntimeData]
  ): Maybe[ElementRuntimeData] = {
    val allTRDs = {
      iter.reset()
      iter.to(LazyList).takeWhile { stackTRD =>
        optTRD.map { _ ne stackTRD }.getOrElse(true)
      }
    }
    //
    // Gather up all possible elements it could be from
    // all stack entries up to, not including this one.
    //
    // We only get here if all the TRDs on stack had nextElementResolvers
    // which passed (returning Nope) to indicate that the elements they
    // had as possibilities are all optional.
    //
    // If we get here, we're at the end of a complex element, and we
    // only get here when the current incoming event was a start element
    //
    // So we have a candidate start element, but it wasn't a match anything
    // possible, and we're expecting an end element for the complex type
    // at this point.
    //
    val allPossibles =
      allTRDs.flatMap {
        _.partialNextElementResolver.currentPossibleNamedQNames
      }
    One(new UnexpectedElementErrorERD(optTRD, name, namespace, allPossibles))
  }
}

/**
 * The schema compiler computes this for each Term in a Sequence.
 *
 * This is for use assembling the Daffodil Infoset from an InfosetInputter.
 * Note that not all InfosetInputter's have a concept of namespaces (json is a
 * prime example of this). In order to support this, each nextElement method
 * has a variable called hasNamespace, which is used to specify whether or not
 * the InfosetInputter supports namespaces. If it does not support namespaces,
 * then the namespace parameter should be ignored. This may affect how the next
 * element is determined, and may result in an UnparseError in some cases.
 */
sealed trait PartialNextElementResolver extends Serializable {

  /**
   * Returns One(ElementRuntimeData) if the arguments are associated with
   * such definition.
   *
   * Returns Nope if there is no such mapping.
   */
  def maybeNextElement(
    name: String,
    nameSpace: String,
    hasNamespace: Boolean
  ): Maybe[ElementRuntimeData]

  // TODO: PERFORMANCE: We really should be interning all QNames so that comparison of QNames can be pointer equality
  // or nearly so. We're going to do tons of lookups in hash tables, which will compute the hash code, find it is equal,
  // compare the entire namespace string character by character, only to say yes, and yes will be by far the vast
  // bulk of the lookup results.  Pointer equality would be so much faster....
  //
  def maybeNextElement(nqn: NamedQName, hasNamespace: Boolean): Maybe[ElementRuntimeData] =
    maybeNextElement(
      nqn.local,
      if (hasNamespace) nqn.namespace.toStringOrNullIfNoNS else null,
      hasNamespace
    )

  def currentPossibleNextElements: Seq[ElementRuntimeData]

  final def currentPossibleNamedQNames = currentPossibleNextElements.map { _.namedQName }
}

/**
 * This resolver exists to allow the ERD of a quasi-element to have a resolver, but
 * it should never actually be used, so this just fails if it is.
 */
class DoNotUseThisResolver(trd: TermRuntimeData) extends PartialNextElementResolver {

  override def maybeNextElement(
    local: String,
    namespace: String,
    hasNamespace: Boolean
  ): Maybe[ElementRuntimeData] =
    Assert.invariantFailed("This resolver should never be used.")

  override def toString() = "DoNotUseThisResolver"

  override val currentPossibleNextElements = Seq()
}

class NoNextElement(trd: TermRuntimeData, isRequiredStreamingUnparserEvent: Boolean)
  extends PartialNextElementResolver {

  override def maybeNextElement(
    local: String,
    namespace: String,
    hasNamespace: Boolean
  ): Maybe[ElementRuntimeData] = {
    if (isRequiredStreamingUnparserEvent)
      One(
        new UnexpectedElementErrorERD(Some(trd), local, namespace, currentPossibleNamedQNames)
      )
    else
      Nope
  }

  override def toString() = "NoNextElement"

  override val currentPossibleNextElements = Seq()

}

class OnlyOnePossibilityForNextElement(
  trd: TermRuntimeData,
  val nextERD: ElementRuntimeData,
  isRequiredStreamingUnparserEvent: Boolean
) extends PartialNextElementResolver {

  val nqn = nextERD.namedQName

  override def maybeNextElement(
    local: String,
    namespace: String,
    hasNamespace: Boolean
  ): Maybe[ElementRuntimeData] = {
    val matches =
      if (hasNamespace) {
        val sqn = StepQName(None, local, NS(namespace))
        sqn.matches(nqn)
      } else {
        // namespace is ignored since the InfosetInputter does not support
        // them. Since there is only one possible match, the local name only
        // has to match the nextERD local name to be a match.
        local == nqn.local
      }

    if (!matches) {
      if (isRequiredStreamingUnparserEvent) {
        One(
          new UnexpectedElementErrorERD(Some(trd), local, namespace, currentPossibleNamedQNames)
        )
      } else {
        Nope
      }
    } else {
      Maybe(nextERD)
    }
  }

  override def toString() = "OnlyOne(" + nqn + ")"

  override lazy val currentPossibleNextElements = Seq(nextERD)
}

class SeveralPossibilitiesForNextElement(
  trd: TermRuntimeData,
  nextERDMap: Map[QNameBase, ElementRuntimeData],
  hasDuplicateLocalNames: Boolean,
  isRequiredStreamingUnparserEvent: Boolean
) extends PartialNextElementResolver {
  Assert.usage(nextERDMap.size > 1, "should be more than one mapping")

  /**
   * Annoying, but scala's immutable Map is not covariant in its first argument
   * the way one would normally expect a collection to be.
   *
   * So Map[StepQName, ElementRuntimeData] is not a subtype of Map[QNameBase, ElementRuntimeData]
   * which means when we construct a Map using the NamedQName of the elements,
   * we can't use that with StepQNames as the query items. But QName comparisons
   * are carefully strongly typed to prevent you from comparing the wrong kinds.
   * For example, you can check if a StepQName matches a NamedQName, but you can't compare
   * two NamedQNames together (because, generally, that would be a mistake.)
   *
   * So we need a cast upward to QNameBase
   */
  override def maybeNextElement(
    local: String,
    namespace: String,
    hasNamespace: Boolean
  ): Maybe[ElementRuntimeData] = {
    Logger.log.debug(s"""
      NextERDResolver -> trd: $trd\n
      NextERDResolver -> looking for: $local\n
      NextERDResolver -> nextERDMap: $nextERDMap\n""")
    val optnextERD =
      if (hasNamespace) {
        val sqn = StepQName(
          None,
          local,
          NS(namespace)
        ) // these will match in a hash table of NamedQNames.
        nextERDMap.get(sqn.asInstanceOf[QNameBase])
      } else {
        // The InfosetInputter does not support namespaces, so we must find an
        // element in the nextERDMap with the same local name. The keys in the
        // map are QNameBase's, so we must instead use a linear search to find
        // the right key
        if (!hasDuplicateLocalNames) {
          // It was statically determined at compile time that this
          // NextElementResolver does not have any possible NextERDs with the
          // same local name. Because of this, just find the first thing that
          // matches and return it if it was found.
          nextERDMap.find(_._1.local == local).map(_._2)
        } else {
          // It was statically determined at compile time that some nextERDs
          // have duplicate local names with differing namespaces. Since this
          // InfosetInputter does not support namespaces, we might not be able
          // to determine which is the right next ERD. So find all nextERDs
          // with a matching local name, and error if we found more than one.
          // If we only found one we found it. If we didn't find any, there was
          // no match.
          val localMatches = nextERDMap.view.filterKeys(_.local == local)
          if (localMatches.size > 1) {
            val sqn = StepQName(None, local, NS(namespace))
            val keys = localMatches.keys.toSeq
            val errERD =
              new NamespaceAmbiguousElementErrorERD(Some(trd), local, namespace, keys)
            errERD.toUnparseError(false)
          } else {
            localMatches.headOption.map(_._2)
          }
        }
      }

    val res = optnextERD
    if (isRequiredStreamingUnparserEvent && res.isEmpty) {
      One(
        new UnexpectedElementErrorERD(Some(trd), local, namespace, currentPossibleNamedQNames)
      )
    } else {
      toMaybe(res)
    }
  }

  override def toString() = "Several(" + nextERDMap.keySet.mkString(", ") + ")"

  override lazy val currentPossibleNextElements = nextERDMap.values.toSeq
}
