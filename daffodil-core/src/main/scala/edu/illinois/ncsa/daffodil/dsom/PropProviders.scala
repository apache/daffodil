package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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


import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.util.Info
import edu.illinois.ncsa.daffodil.util.Debug
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropertyMixin

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
trait LeafPropProvider
  extends LookupLocation with PropTypes with Logging {

  /**
   * for debug/test only
   */
  lazy val properties: PropMap = justThisOneProperties

  //TODO: optimize by having this object actually check if there
  // are any property bindings. if not, this should get removed 
  // from any Seq[LeafPropProvider] it is put into.

  def prettyName: String

  /**
   * properties for just this object, but they can be
   * short, long, or element forms.
   */
  def justThisOneProperties: PropMap

  final def leafFindProperty(pname: String): PropertyLookupResult = {
    log(Debug("%s leafFindProperty %s on %s", prettyName, pname, this))
    val mine = justThisOneProperties.get(pname)
    val res = mine match {
      case Some((value, loc)) => Found(value, loc)
      case None => NotFound(List(this), Nil)
    }
    log(Debug("%s leafFindProperty %s ", prettyName, res))
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
class ChainPropProvider(leafProvidersArg: Seq[LeafPropProvider], forAnnotation: DFDLAnnotation)
  extends Logging with PropTypes {

  /**
   * for debug/test only
   */
  lazy val properties: PropMap = leafProviders.flatMap { _.properties.toSeq }.toMap

  val leafProviders = leafProvidersArg

  def prettyName: String = "ChainPropProvider(" + forAnnotation.prettyName + ")"

  final def chainFindProperty(pname: String): PropertyLookupResult = {
    log(Debug("%s chainFindProperty %s.", prettyName, pname))
    lookupPropertyInSources(leafProviders, pname)
  }

  /*
   * The Override algorithm - first property source with a hit wins.
   */
  def lookupPropertyInSources(sources: Seq[LeafPropProvider], pname: String): PropertyLookupResult = {
    val allNotFound =
      for { source <- sources } yield {
        val res = source.leafFindProperty(pname)
        res match {
          case Found(_, _) => return res //found it! return right now.
          case nf @ NotFound(_, _) => nf
        }
      }
    // didn't find it. Compile complete list of everywhere we looked.
    val allLocalPlacesSearched = allNotFound.flatMap { _.localWhereLooked }.toSeq
    NotFound(allLocalPlacesSearched, Seq())
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

  def checkNonOverlap(providers: Seq[ChainPropProvider]) = {

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
  def checkNonOverlap1(list: List[ChainPropProvider]): Unit = {
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
  def checkNonOverlap2(a: ChainPropProvider, b: List[ChainPropProvider]) = {
    b.foreach { bCPP => checkNonOverlap3(a, bCPP) }
  }

  /**
   * iterate through all properties of the a-arg, checking each for
   * whether it appears in the b-arg. If so, SDE.
   */
  def checkNonOverlap3(a: ChainPropProvider, b: ChainPropProvider) {
    val aLeaves = a.leafProviders
    aLeaves.foreach { aLeaf =>
      val propMap = aLeaf.justThisOneProperties
      propMap.foreach {
        case (propName, (_, aLoc)) =>
          b.chainFindProperty(propName) match {
            case _: NotFound => // ok
            case Found(_, bLoc) => {
              schemaDefinitionErrorButContinue(
                "Overlapping properties: %1$s overlaps between %2$s and %3$s. Overlap is not allowed.",
                propName, aLoc, bLoc)
            }
          }
      }
    }
  }
}
