/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import scala.xml.Node
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropertyLookupResult
import edu.illinois.ncsa.daffodil.schema.annotation.props.NotFound
import edu.illinois.ncsa.daffodil.schema.annotation.props.Found
import edu.illinois.ncsa.daffodil.schema.annotation.props.FindPropertyMixin

/**
 * Only objects from which we generate processors (parsers/unparsers)
 * can lookup property values.
 *
 * This avoids the possibility of a property being resolved incorrectly by
 * not looking at the complete chain of schema components contributing to the
 * property resolution.
 *
 * The only objects that should resolve properties are
 * ElementRef, Root, LocalElementDecl, Sequence, Choice, SequenceRef, ChoiceRef
 *
 * These are all the "real" terms. Everything else is just contributing
 * properties to the mix, but they are not points where properties are
 * used to generate processors.
 */
trait ResolvesProperties
  extends FindPropertyMixin { self: AnnotatedSchemaComponent =>

  def term: Term

  private def findNonDefaultProperty(pname: String): PropertyLookupResult = {
    val result = findDefaultOrNonDefaultProperty(pname, nonDefaultPropertySources)
    result match {
      case f: Found => f
      case NotFound(nd, d, _) =>
        Assert.invariant(d.isEmpty)
    }
    result
  }

  private def findDefaultProperty(pname: String): PropertyLookupResult = {
    val result = findDefaultOrNonDefaultProperty(pname, defaultPropertySources)
    val fixup = result match {
      case Found(value, loc, pname, _) =>
        // found as a default property.
        // supply constructor's last arg is boolean indicating it's a default property
        Found(value, loc, pname, true)
      case NotFound(nd, d, pn) =>
        Assert.invariant(d.isEmpty)
        NotFound(Seq(), nd, pn) // we want the places we searched shown as default locations searched
    }
    fixup
  }

  override def findPropertyOption(pname: String): PropertyLookupResult = {
    ExecutionMode.requireCompilerMode
    // first try in regular properties
    val regularResult = findNonDefaultProperty(pname)
    regularResult match {
      case f: Found => f
      case NotFound(nonDefaultLocsTried1, defaultLocsTried1, _) => {
        Assert.invariant(defaultLocsTried1.isEmpty)
        val defaultResult = findDefaultProperty(pname)
        defaultResult match {
          case f: Found => f
          case NotFound(nonDefaultLocsTried2, defaultLocsTried2, _) => {
            Assert.invariant(nonDefaultLocsTried2.isEmpty)
            // did not find it at all. Return a NotFound with all the places we
            // looked non-default and default.
            val nonDefaultPlaces = nonDefaultLocsTried1
            val defaultPlaces = defaultLocsTried2
            NotFound(nonDefaultPlaces, defaultPlaces, pname)
          }
        }
      }
    }
  }
}

abstract class AnnotatedSchemaComponentImpl( final override val xml: Node,
  final override val parent: SchemaComponent)
  extends AnnotatedSchemaComponent

/**
 * Shared characteristics of any annotated schema component.
 * Not all components can carry DFDL annotations.
 */
trait AnnotatedSchemaComponent
  extends SchemaComponent
  with AnnotatedMixin
  with OverlapCheckMixin {

  final lazy val term = this match {
    case t: Term => t
    case ged: GlobalElementDecl => ged.elementRef
    case ty: SimpleTypeDefBase => ty.elementBase
    case ty: ComplexTypeBase => ty.elementBase
    case sd: SchemaDocument =>
      Assert.usageError("not to be called for schema documents")
  }

  final lazy val resolver: ResolvesProperties = this match {
    case sd: SchemaDocument => sd
    case _ => term
  }

  requiredEvaluations(annotationObjs)
  requiredEvaluations(shortFormPropertiesCorrect)
  requiredEvaluations(nonDefaultPropertySources)
  requiredEvaluations(defaultPropertySources)

  //  /**
  //   * only used for debugging
  //   */
  //  override lazy val properties: PropMap =
  //    (nonDefaultPropertySources.flatMap { _.properties.toSeq } ++
  //      defaultPropertySources.flatMap { _.properties.toSeq }).toMap

  final lazy val shortFormPropertiesCorrect: Boolean = {
    // Check that any unprefixed properties are disjoint with ALL DFDL property names.
    // Warning otherwise
    // Insure that some prefix is bound to the dfdl namespace. Warn otherwise.
    // Warn if dfdl: is bound to something else than the DFDL namespace.
    shortFormAnnotationsAreValid
  }

  /**
   * Since validation of extra attributes on XML Schema elements is
   * normally lax validation, we can't count on validation of DFDL schemas
   * to tell us whether short-form annotations are correct or not.
   *
   * So, we have to do this check ourselves.
   *
   * TBD: change properties code generator to output the various lists of
   * properties that we have to check against. (Might already be there...?)
   *
   */
  def shortFormAnnotationsAreValid: Boolean = true // TODO: Implement this - DFDL-598, DFDL-1512

  /**
   * For property combining only. E.g., doesn't refer from an element
   * to its complex type because we don't combine properties with that
   * in DFDL v1.0. (I consider that a language design bug in DFDL v1.0, but
   * that is the way it's defined.)
   */
  final protected def refersToForPropertyCombining: Option[AnnotatedSchemaComponent] = optReferredToComponent

  protected def optReferredToComponent: Option[AnnotatedSchemaComponent] = None // override in ref objects

  final protected lazy val nonDefaultPropertySources: Seq[ChainPropProvider] = LV('nonDefaultPropertySources) {
    this match {
      case sd: SchemaDocument => Nil
      case _ => {
        val refTo = refersToForPropertyCombining
        val chainFromReferredTo = refTo.map { _.nonDefaultPropertySources }.toSeq.flatten
        val completeNonDefaultFormatChain = nonDefaultFormatChain +: chainFromReferredTo
        val seq = completeNonDefaultFormatChain.distinct
        checkNonOverlap(seq)
        seq
      }
    }
  }.value

  final protected lazy val defaultPropertySources: Seq[ChainPropProvider] = LV('defaultPropertySources) {
    val refTo = refersToForPropertyCombining
    val chainFromReferredTo = refTo.toSeq.map { _.defaultPropertySources }.distinct.flatten
    val completeDefaultFormatChain = defaultFormatChain +: chainFromReferredTo
    val seq = completeDefaultFormatChain.distinct
    seq
  }.value

  final protected lazy val nonDefaultFormatChain: ChainPropProvider = {
    val fa = formatAnnotation
    val fc = fa.formatChain
    fc
  }

  private lazy val defaultFormatChain: ChainPropProvider = {
    val res = schemaDocument.formatAnnotation.formatChain
    res
  }

  protected final def findDefaultOrNonDefaultProperty(
    pname: String,
    sources: Seq[ChainPropProvider]): PropertyLookupResult = {
    //
    // Important - use of stream here insures we don't lookup
    // properties down the chain once we have them here.
    //
    val str = sources.toStream.map { _.chainFindProperty(pname) }
    val optFound = str.collectFirst { case found: Found => found }
    val result = optFound match {
      case Some(f: Found) => f
      case None => {
        val seq = str.toSeq
        // merge all the NotFound stuff.
        val nonDefaults = seq.flatMap {
          case NotFound(nd, d, _) => nd
          case _: Found => Assert.invariantFailed()
        }
        val defaults = seq.flatMap {
          case NotFound(nd, d, _) => d
          case _: Found => Assert.invariantFailed()
        }
        Assert.invariant(defaults.isEmpty)
        val nf = NotFound(nonDefaults, defaults, pname)
        nf
      }
    }
    result
  }

  /**
   * Use this when you want to know if a property is defined exactly on a
   * component. This ignores any default properties or properties defined on
   * element references. For example, if you want to know if a property was
   * defined on a global element decl rather than an element reference to that
   * decl.
   */
  final def findPropertyOptionThisComponentOnly(pname: String): PropertyLookupResult = {
    val result = findDefaultOrNonDefaultProperty(pname, Seq(nonDefaultFormatChain))
    result
  }
}

/**
 * Every component that can be annotated.
 * Review Note:
 * It's no longer clear that this separation is strictly speaking needed.
 * It's possible that this could be collapsed back into AnnotatedSchemaComponent
 * or made smaller anyway.
 *
 */
trait AnnotatedMixin
  extends EscapeSchemeRefMixin { self: AnnotatedSchemaComponent =>

  /**
   * Anything annotated must be able to construct the
   * appropriate DFDLAnnotation object from the xml.
   */
  protected def annotationFactory(node: Node): Option[DFDLAnnotation]

  private lazy val annotationNode = {
    val ann = xml \ "annotation"
    ann
  }

  /**
   * dais = Dfdl App Info nodeSeq
   */
  private lazy val dais = {
    val ais = (annotationNode \ "appinfo")
    val dais = ais.filter { ai =>
      {
        ai.attribute("source") match {
          case None => {
            this.SDW("""xs:appinfo without source attribute. Is source="http://www.ogf.org/dfdl/" missing?""")
            false
          }
          case Some(n) => {
            val sourceNS = NS(n.text)
            //
            // Keep in mind. As the DFDL standard evolves, and new versions
            // come out, this code may change to tolerate different source
            // attributes that call out distinct versions of the standard.
            //
            val officialAppinfoSourceAttributeNS = XMLUtils.dfdlAppinfoSource
            //
            // Note: use of the strongly typed =:= operator below.
            //
            // I got sick of mysterious behavior where turns out we are
            // comparing two things of different types.
            //
            // This fixes a bug where we were comparing a string to a NS
            // and getting false, where the types should have been the same.
            //
            val hasRightSource = (sourceNS =:= officialAppinfoSourceAttributeNS)
            val isAcceptable = sourceNS.toString.contains("ogf") && sourceNS.toString.contains("dfdl")
            schemaDefinitionWarningWhen(!hasRightSource && isAcceptable,
              "The xs:appinfo source attribute value '%s' should be '%s'.", sourceNS, officialAppinfoSourceAttributeNS)
            (hasRightSource || isAcceptable)
          }
        }
      }
    }
    dais
  }

  /**
   * The DFDL annotations on the component, as objects
   * that are subtypes of DFDLAnnotation.
   */
  final lazy val annotationObjs = {
    // println(dais)
    val objs = dais.flatMap { dai =>
      {
        val children = dai.child
        val res = children.filter { _.isInstanceOf[scala.xml.Elem] }.flatMap { child =>
          {
            annotationFactory(child)
          }
        }
        res
      }
    }
    objs
  }

  /**
   * Here we establish an invariant which is that every annotatable schema component has, definitely, has an
   * annotation object. It may have no properties on it, but it will be there. Hence, we can
   * delegate various property-related attribute calculations to it.
   *
   * To realize this, every concrete class must implement (or inherit) an implementation of
   * emptyFormatFactory, which constructs an empty format annotation,
   * and isMyFormatAnnotation which tests if an annotation is the corresponding kind.
   *
   * Given that, formatAnnotation then either finds the right annotation, or constructs one, but our invariant
   * is imposed. There *is* a formatAnnotation.
   */
  protected def emptyFormatFactory: DFDLFormatAnnotation
  protected def isMyFormatAnnotation(a: DFDLAnnotation): Boolean

  final lazy val formatAnnotation = LV('formatAnnotation) {
    val format = annotationObjs.collect { case fa: DFDLFormatAnnotation if isMyFormatAnnotation(fa) => fa }
    val res = format match {
      case Seq() => emptyFormatFactory // does make things with the right namespace scopes attached!
      case Seq(fa) => fa
      case _ => schemaDefinitionError("Only one format annotation is allowed at each annotation point.")
    }
    res
  }.value

}
