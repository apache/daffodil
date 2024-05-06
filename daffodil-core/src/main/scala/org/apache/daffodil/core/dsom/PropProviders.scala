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

package org.apache.daffodil.core.dsom

import org.apache.daffodil.lib.schema.annotation.props.Found
import org.apache.daffodil.lib.schema.annotation.props.LookupLocation
import org.apache.daffodil.lib.schema.annotation.props.NotFound
import org.apache.daffodil.lib.schema.annotation.props.PropTypes
import org.apache.daffodil.lib.schema.annotation.props.PropertyLookupResult
import org.apache.daffodil.lib.util._

/**
 * Property lookup uses ChainPropProviders containing LeafPropProviders.
 *
 * Each ChainPropProvider is at its core a Seq[LeafPropProvider] created
 * by following the dfdl:ref attribute 'chain' of a DFDLFormatAnnotation from
 * one format annotation to another by way of a named define format.
 *
 * A LeafPropProvider corresponds directly to a DFDLFormatAnnotation. It
 * can supply properties from the short, long, and element form property bindings
 * found on that object alone. It insures they are non-overlapping.
 *
 * Ultimately, a schema component will have two sequences of ChainPropProviders.
 * One for non-default properties (these must be tested to be sure they are
 * non-overlapping), and one for default properties. Those are assembed by
 * each schema component in a component-specific manner.
 */

/**
 * A single annotation which combines short form, long form,
 * and element form property bindings together.
 *
 * From this perspective, there are no ref chains connecting
 * format annotations together.
 */
trait LeafPropProvider extends LookupLocation with PropTypes {

  /**
   * for debug/test only
   */
  final lazy val properties: PropMap = justThisOneProperties

  // TODO: optimize by having this object actually check if there
  // are any property bindings. if not, this should get removed
  // from any Seq[LeafPropProvider] it is put into.

  def diagnosticDebugName: String

  /**
   * properties for just this object, but they can be
   * short, long, or element forms.
   */
  def justThisOneProperties: PropMap

  /**
   * Just the properties as string pairs, without any location information.
   *
   * Intended for use comparing to see if two prop providers are equivalent
   * in terms of providing the same property values.
   */
  final lazy val justThisOnePropertyPairsSet = justThisOneProperties.map { case (s1, (s2, _)) =>
    (s1, s2)
  }.toSet

  final def leafFindProperty(pname: String): PropertyLookupResult = {
    val mine = justThisOneProperties.get(pname)
    val res = mine match {
      case Some((value, loc)) => {
        //
        // One downside of the ConstructingParser for XML - we lose the
        // schema-aware processing that was trimming off whitespace
        // from properties/attributes declared as xs:tokens automatically.
        //
        // Instead we have to implement our own trim logic on a
        // case by case basis. (which means not here)
        // Ex: textNumberPattern spaces are significant and important to preserve
        // AND tend to be leading or trailing in the string value. So if anyone
        // does string.trim on that property value, they will be clobbering it.
        //
        Found(value, loc, pname, false)
      }
      case None => NotFound(List(this), Nil, pname)
    }
    res
  }

}

/**
 * flat chain of format annotations connected by dfdl:ref (short form)
 * or ref (long form) references to named defined formats.
 *
 * Could be the nonDefault formats being chained together, or could
 * be the default formats being chained together.
 */
final class ChainPropProvider(leafProvidersArg: Seq[LeafPropProvider], forAnnotation: String)
  extends PropTypes {

  override def toString() = Misc.getNameFromClass(this) + "(" + forAnnotation + ")"

  /**
   * This is a sequence of sets basically for debug/maintenance reasons.
   * Conceptually it could be flattened to just a set, but this way the contents of each set
   * may be visibly meaningful when debugging, as the first set comes from the
   * first leaf provider, second from the second (which comes from first dfdl:ref
   * reference), etc.
   */
  final lazy val propertyPairsSets: Seq[Set[(String, String)]] =
    leafProviders.map { _.justThisOnePropertyPairsSet }

  /**
   * for debug/test only
   */
  final lazy val properties: PropMap = leafProviders.flatMap { _.properties.toSeq }.toMap

  final lazy val leafProviders = leafProvidersArg

  final def chainFindProperty(pname: String): PropertyLookupResult = {
    lookupPropertyInSources(leafProviders, pname)
  }

  /*
   * The Override algorithm - first property source with a hit wins.
   */
  private def lookupPropertyInSources(
    sources: Seq[LeafPropProvider],
    pname: String
  ): PropertyLookupResult = {
    val allNotFound =
      for { source <- sources } yield {
        val res = source.leafFindProperty(pname)
        res match {
          case _: Found => return res // found it! return right now.
          case nf @ NotFound(_, _, _) => nf
        }
      }
    // didn't find it. Compile complete list of everywhere we looked.
    val allLocalPlacesSearched = allNotFound.flatMap { _.localWhereLooked }.toSeq
    NotFound(allLocalPlacesSearched, Seq(), pname)
  }
}

// TODO: Check for circularity if schema validation doesn't.
// That is, we check for circularity in the ref chain from a
// format annotation to a defineFormat.
//
// What we may need to check is for recursive circularity of
// the chains from say, simpleTypes to their bases.
//
// There are other funny circularities possible also. E.g,
// a sequence contains a group ref, to a group def, which contains
// the original sequence.
// Element refs and elements can set up the same sort of
// nesting cycle.
// A ComplexType can contain an element which has that same
// ComplexType.

trait OverlapCheckMixin {

  def schemaDefinitionErrorButContinue(str: String, args: Any*): Unit

  /**
   * check for overlap.
   */

  protected final def checkNonOverlap(providers: Seq[ChainPropProvider]) = {

    /**
     * given a list, take the first, make sure it is non-overlapping with
     * each of the rest.
     *
     * So if we have the whole list, that tells us the first doesn't overlap with
     * any.
     *
     * Then we need the first list tail, and checking that will tell us that the
     * second does not overlap with any of the rest.
     *
     * And so on for second list tail to check third against rest,
     *
     * So to do this, we need to process the whole list and each of its tails
     * I.e, given a list x, we want List(x, x.tail, x.tail.tail, ....)
     *
     */
    def allTails[T](list: List[T]): List[List[T]] = {
      list match {
        case Nil => Nil
        case _ :: r => list :: allTails(r)
      }
    }

    // Detail: let's reverse the list. So it will detect overlaps deep in the
    // SimpleTypeDef base chains first, then elementDecl to SimpleTypeDef chain, then
    // finally ElementRef to ElementDecl and SimpleTypeDef chain.
    val tails = allTails(providers.toList).reverse

    tails.foreach { checkNonOverlap1(_) }

  }

  /**
   * Check first of the list for overlap against each of the 2nd through last
   */
  private def checkNonOverlap1(list: List[ChainPropProvider]): Unit = {
    list match {
      case Nil => // ok
      case List(_) => // ok. One thing can't overlap with anything.
      case fCPP :: rList => {
        checkNonOverlap2(fCPP, rList)
        checkNonOverlap1(rList)
      }
    }
  }

  /**
   * For each in the b argument list, check that the a arg doesn't overlap
   * with it.
   */
  private def checkNonOverlap2(a: ChainPropProvider, b: List[ChainPropProvider]) = {
    b.foreach { bCPP => checkNonOverlap3(a, bCPP) }
  }

  /**
   * iterate through all properties of the a-arg, checking each for
   * whether it appears in the b-arg. If so, SDE.
   */
  private def checkNonOverlap3(a: ChainPropProvider, b: ChainPropProvider): Unit = {
    val aLeaves = a.leafProviders
    aLeaves.foreach { aLeaf =>
      val propMap = aLeaf.justThisOneProperties
      propMap.foreach { case (propName, (_, aLoc)) =>
        b.chainFindProperty(propName) match {
          case _: NotFound => // ok
          case Found(_, bLoc, _, _) => {
            schemaDefinitionErrorButContinue(
              "Overlapping properties: %1$s overlaps between %2$s and %3$s. Overlap is not allowed.",
              propName,
              aLoc,
              bLoc
            )
          }
        }
      }
    }
  }
}
