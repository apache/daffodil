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

package org.apache.daffodil.infoset

import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.unparsers.UnparseError
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.StepQName
import org.apache.daffodil.xml.QNameBase
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.xml.NamedQName

/**
 * The schema compiler computes this for each element.
 *
 * This is for use assembling the Daffodil Infoset from an InfosetInputter.
 * Note that not all InfosetInputter's have a concept of namespaces (json is a
 * prime example of this). In order to support this, each nextElement method
 * has a variable called hasNamespace, which is used to specify whether or not
 * the InfosetInputter supports namespaces. If it does not support namespaces,
 * then the namespace parameter should be ignored. This may affect how the next
 * element is determined, and may result in an UnparseError in some cases.
 */

trait NextElementResolver extends Serializable {

  def nextElement(name: String, nameSpace: String, hasNamespace: Boolean): ElementRuntimeData

  // TODO: PERFORMANCE: We really should be interning all QNames so that comparison of QNames can be pointer equality
  // or nearly so. We're going to do tons of lookups in hash tables, which will compute the hash code, find it is equal,
  // compare the entire namespace string character by character, only to say yes, and yes will be by far the vast
  // bulk of the lookup results.  Pointer equality would be so much faster....
  //
  def nextElement(nqn: NamedQName, hasNamespace: Boolean): ElementRuntimeData =
    nextElement(nqn.local, if (hasNamespace) nqn.namespace.toStringOrNullIfNoNS else null, hasNamespace)
    
  def allPossibleNextElements: Seq[ElementRuntimeData]
}

sealed abstract class ResolverType(val name: String) extends Serializable
case object SiblingResolver extends ResolverType("next")
case object ChildResolver extends ResolverType("child")
case object RootResolver extends ResolverType("root")

class NoNextElement(schemaFileLocation: SchemaFileLocation, resolverType: ResolverType) extends NextElementResolver {

  override def nextElement(local: String, namespace: String, hasNamespace: Boolean): ElementRuntimeData = {
    val sqn = StepQName(None, local, NS(namespace))
    UnparseError(One(schemaFileLocation), Nope, "Found %s element %s, but no element is expected.", resolverType.name, sqn)
  }

  override def toString() = "NoNextElement"
  
  override val allPossibleNextElements = Seq()

}

class OnlyOnePossibilityForNextElement(schemaFileLocation: SchemaFileLocation, val nextERD: ElementRuntimeData, resolverType: ResolverType)
  extends NextElementResolver {

  override def nextElement(local: String, namespace: String, hasNamespace: Boolean): ElementRuntimeData = {
    val nqn = nextERD.namedQName
    val matches =
      if (hasNamespace) {
        val sqn = StepQName(None, local, NS(namespace))
        sqn.matches(nqn)
      } else {
        // namespace is ignored since the InfosetInputter does not support
        // them. Since there is only one possible match, the local name only
        // has to match the nextERD local name to be a match.
        local == nextERD.namedQName.local
      }

    if (!matches) {
      val sqn = StepQName(None, local, NS(namespace))
      UnparseError(One(schemaFileLocation), Nope, "Found %s element %s, but expected %s.", resolverType.name,
        sqn.toExtendedSyntax, nqn.toExtendedSyntax)
    }

    nextERD
  }

  override def toString() = "OnlyOne(" + nextERD.namedQName + ")"
  
  override lazy val allPossibleNextElements = Seq(nextERD)
}

/**
 * Schema compiler computes the map here, and then attaches this object to the
 * ERD of each element.
 */
class SeveralPossibilitiesForNextElement(loc: SchemaFileLocation, nextERDMap: Map[QNameBase, ElementRuntimeData], resolverType: ResolverType, hasDuplicateLocalNames: Boolean)
  extends NextElementResolver {
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
  override def nextElement(local: String, namespace: String, hasNamespace: Boolean): ElementRuntimeData = {
    val optNextERD =
      if (hasNamespace) {
        val sqn = StepQName(None, local, NS(namespace)) // these will match in a hash table of NamedQNames.
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
          // matches and return it it is was found.
          nextERDMap.find(_._1.local == local).map(_._2)
        } else {
          // It was statically determined at compile time that some nextERDs
          // have duplicate local names with differing namespaces. Since this
          // InfosetInputter does not support namespaces, we might not be able
          // to determine which is the right next ERD. So find all nextERDs
          // with a matching local name, and error if we found more than one.
          // If we only found one we found it. If we didn't find any, there was
          // no match.
          val localMatches = nextERDMap.filterKeys(_.local == local)
          if (localMatches.size > 1) {
            val sqn = StepQName(None, local, NS(namespace))
            val keys = localMatches.keys.toSeq
            UnparseError(One(loc), Nope, "Found multiple matches for %s element %s because infoset implementation ignores namespaces. Matches are %s",
              resolverType.name, sqn.toExtendedSyntax, keys.mkString(", "))
          }
          localMatches.headOption.map(_._2)
        }
      }

    val res = optNextERD.getOrElse {
      val keys = nextERDMap.keys.toSeq
      val sqn = StepQName(None, local, NS(namespace))
      UnparseError(One(loc), Nope, "Found %s element %s, but expected one of %s.",
        resolverType.name, sqn.toExtendedSyntax, keys.mkString(", "))
    }
    res
  }

  override def toString() = "Several(" + nextERDMap.keySet.mkString(", ") + ")"
  
  override lazy val allPossibleNextElements = nextERDMap.values.toSeq
}
