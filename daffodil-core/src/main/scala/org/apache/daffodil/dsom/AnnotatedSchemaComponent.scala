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

package org.apache.daffodil.dsom

import scala.xml.Node
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.ExecutionMode
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.xml.NS
import org.apache.daffodil.equality._
import org.apache.daffodil.schema.annotation.props.PropertyLookupResult
import org.apache.daffodil.schema.annotation.props.NotFound
import org.apache.daffodil.schema.annotation.props.Found
import org.apache.daffodil.schema.annotation.props.FindPropertyMixin
import org.apache.daffodil.api.WarnID

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

  /**
   * Does lookup of property using DFDL scoping rules, checking first non-default
   *  properties, then default property locations.
   */
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

/** Convenience class for implemening AnnotatedSchemaComponent trait */
abstract class AnnotatedSchemaComponentImpl(
  final override val xml: Node,
  final override val parent: SchemaComponent)
  extends AnnotatedSchemaComponent

/**
 * Shared characteristics of any annotated schema component.
 *
 * Not all components can carry DFDL annotations.
 */
trait AnnotatedSchemaComponent
  extends SchemaComponent
  with AnnotatedMixin
  with OverlapCheckMixin {

  requiredEvaluations(annotationObjs)
  requiredEvaluations(nonDefaultPropertySources)
  requiredEvaluations(defaultPropertySources)

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
  // TODO: Implement this - DFDL-598, DFDL-1512
  // private def areShortFormAnnotationsValid: Boolean = true

  /**
   * For property combining only. E.g., doesn't refer from an element
   * to its complex type because we don't combine properties with that
   * in DFDL v1.0. (I consider that a language design bug in DFDL v1.0, but
   * that is the way it's defined.)
   */
  final protected def refersToForPropertyCombining: Option[AnnotatedSchemaComponent] = optReferredToComponent

  protected def optReferredToComponent: Option[AnnotatedSchemaComponent] // override in ref objects

  lazy val nonDefaultPropertySources: Seq[ChainPropProvider] = LV('nonDefaultPropertySources) {
    this match {
      case sd: SchemaDocument => Nil
      case _ => {
        val refTo = refersToForPropertyCombining
        val chainFromReferredTo = refTo.map { c =>
          val ndps = c.nonDefaultPropertySources
          ndps
        }.toSeq.flatten
        val myNDFC = nonDefaultFormatChain
        val completeNonDefaultFormatChain =
          myNDFC +: chainFromReferredTo
        val seq = completeNonDefaultFormatChain.distinct
        checkNonOverlap(seq)
        seq
      }
    }
  }.value

  final protected lazy val defaultPropertySources: Seq[ChainPropProvider] = LV('defaultPropertySources) {
    val refTo = refersToForPropertyCombining
    val chainFromReferredTo = refTo.toSeq.map { _.defaultPropertySources }.distinct.flatten
    val completeDefaultFormatChain =
      defaultFormatChain +: chainFromReferredTo
    val seq = completeDefaultFormatChain.distinct
    seq
  }.value

  final protected lazy val nonDefaultFormatChain: ChainPropProvider = {
    val fa = formatAnnotation
    val fc = fa.formatChain
    fc
  }

  final protected lazy val defaultFormatChain: ChainPropProvider = {
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
trait AnnotatedMixin { self: AnnotatedSchemaComponent =>

  /**
   * Anything annotated must be able to construct the
   * appropriate DFDLAnnotation object from the xml.
   */
  protected def annotationFactory(node: Node): Option[DFDLAnnotation]

  private lazy val annotationNode = {
    val ann = xml \ "annotation"
    ann
  }

  lazy val dfdlAppInfos = {
    val ais = (annotationNode \ "appinfo")
    val dais = ais.filter { ai =>
      {
        ai.attribute("source") match {
          case None => {
            this.SDW(WarnID.AppinfoNoSource, """xs:appinfo without source attribute. Is source="http://www.ogf.org/dfdl/" missing?""")
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
            schemaDefinitionWarningWhen(WarnID.AppinfoDFDLSourceWrong, !hasRightSource && isAcceptable,
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
  final lazy val annotationObjs = LV('annotationObjs) {
    val objs = dfdlAppInfos.flatMap { dai =>
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
  }.value

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

  final lazy val formatAnnotationExpectedName = emptyFormatFactory.xml.asInstanceOf[scala.xml.Elem].label

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
