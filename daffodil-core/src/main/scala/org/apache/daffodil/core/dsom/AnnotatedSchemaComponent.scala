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

import scala.collection.mutable
import scala.xml.Node
import scala.xml.NodeSeq

import org.apache.daffodil.lib.api.WarnID
import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.FindPropertyMixin
import org.apache.daffodil.lib.schema.annotation.props.Found
import org.apache.daffodil.lib.schema.annotation.props.NotFound
import org.apache.daffodil.lib.schema.annotation.props.PropertyLookupResult
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.XMLUtils

/**
 * Only objects from which we generate processors (parsers/unparsers)
 * can lookup scoped property values.
 *
 * This avoids the possibility of a property being resolved incorrectly by
 * not looking at the complete chain of schema components contributing to the
 * property resolution.
 *
 * The only objects that should resolve properties are instances of Term
 * ElementRef, Root, LocalElementDecl, Sequence, Choice, SequenceRef, ChoiceRef,
 * EnumerationFactory, SimpleTypeDefFactory,
 *
 * All "real" terms are able to resolve properties. Most other objects just contribute
 * properties to the mix, but they are not points where properties are
 * used to generate processors.
 */

object ResolvesProperties {

  /**
   * List of properties that when looked up should only be found on the
   * immediate element--properties on a reference or defaults should not be
   * taken into account.
   */
  val localOnlyProperties = Seq(
    "choiceBranchKey",
    "hiddenGroupRef",
    "repType",
    "repValueRanges",
    "repValues",
  )
}

/**
 * Mixin for non-terms that need to lookup local properties
 */
trait ResolvesLocalProperties extends FindPropertyMixin { self: AnnotatedSchemaComponent =>

  /**
   * Does lookup of only local properties
   */
  protected override def lookupProperty(pname: String): PropertyLookupResult = {
    Assert.usage(
      ResolvesProperties.localOnlyProperties.contains(pname),
      "Property '%s' is not a valid local-only property.".format(pname),
    )
    val fa = formatAnnotation
    val opt = fa.justThisOneProperties.get(pname)
    val optFound = opt.map { case (value, location) => Found(value, location, pname, false) }
    val res = optFound.getOrElse { NotFound(Seq(fa.annotatedSC), Nil, pname) }
    res
  }
}

/**
 * Mixin for Term, which can lookup all properties using DFDL scoping rules.
 */
trait ResolvesScopedProperties extends FindPropertyMixin { self: Term =>

  private def findNonDefaultProperty(pname: String): PropertyLookupResult = {
    val sources =
      if (ResolvesProperties.localOnlyProperties.contains(pname)) {
        Seq(nonDefaultFormatChain)
      } else {
        nonDefaultPropertySources
      }

    val result = findPropertyInSources(pname, sources)
    result match {
      case f: Found => f
      case NotFound(nd, d, _) =>
        Assert.invariant(d.isEmpty)
    }
    result
  }

  private def findDefaultProperty(pname: String): PropertyLookupResult = {
    val result = findPropertyInSources(pname, defaultPropertySources)
    val fixup = result match {
      case Found(value, loc, pname, _) =>
        // found as a default property.
        // supply constructor's last arg is boolean indicating it's a default property
        Found(value, loc, pname, true)
      case NotFound(nd, d, pn) =>
        Assert.invariant(d.isEmpty)
        NotFound(
          Seq(),
          nd,
          pn,
        ) // we want the places we searched shown as default locations searched
    }
    fixup
  }

  private def findPropertyInSources(
    pname: String,
    sources: Seq[ChainPropProvider],
  ): PropertyLookupResult = {
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
   * Does lookup of property using DFDL scoping rules, checking first non-default
   *  properties, then default property locations.
   */
  protected override def lookupProperty(pname: String): PropertyLookupResult = {
    // first try in regular properties
    val regularResult = findNonDefaultProperty(pname)
    regularResult match {
      case f: Found => f
      case nf @ NotFound(nonDefaultLocsTried1, defaultLocsTried1, _) => {
        if (ResolvesProperties.localOnlyProperties.contains(pname)) {
          nf
        } else {
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
}

/** Convenience class for implemening AnnotatedSchemaComponent trait */
abstract class AnnotatedSchemaComponentImpl(
  final override val xml: Node,
  final override val optLexicalParent: Option[SchemaComponent],
) extends AnnotatedSchemaComponent {

  def this(xml: Node, lexicalParent: SchemaComponent) =
    this(xml, Option(lexicalParent))
}

/**
 * Identifies the property environment of a term.
 *
 * If two terms have the same propEnv and same def/decl, then some things can be shared
 * about their implementation.
 *
 * If they have different propEnv, then all bets are off.
 *
 * Can be used as a key for whether to create new instance in a factory, or whether one
 * can be re-used because the property environment is the same.
 *
 * The trick is that the regular objects which carry properties have location information
 * unique to where they originate (for diagnostic messaging purposes).
 *
 * What we want is pure value based - depends on the properties and their values only.
 *
 * The components are of type Seq[Set[String, String]] because the machinery below has
 * alredy converted dfdl:ref="q:name" format references into references to objects having
 * property resolvers. It is easiest just to extract the string-to-string information rather than
 * trying to reuse the named format objects directly.
 *
 * These Seq[Set[String, String]] are conceptually equivalent to just property sets,
 * but keeping them ordered like this preserves some information for us which
 * might be useful in debugging/maintenance of the code. the first set in
 * the sequence are the local properties excluding dfdl:ref, the next set is those from
 * the dfdl:ref, and the next from the next dfdl:ref, and so on. For purposes of
 * a hash key we don't care that they are flattened. that happens elsewhere. It will
 * be much easier to see why two PropEnv objects are not equal looking at this than an
 * entirely flattened set of properties, most of which aren't relevant.
 *
 * optNext is the optional next schema component. So for an element reference, it is the
 * global element declaration. For a group reference, it is the group definition. It is the
 * next place we would get properties from (and defaults from) for property resolution.
 */
case class PropEnv(
  localProps: Seq[Set[(String, String)]],
  defaultPropSource: Seq[Set[(String, String)]],
  optNext: Option[scala.xml.Node],
)

case class ShareKey(xml: scala.xml.Node, env: PropEnv) {

  /**
   * It is critical here that we perform reference equality on the XML. This is
   * because it is possible that the same piece of XML with the same properties
   * could appear in two completely different contexts. If we perform object
   * equality, then we might end up sharing parts of the grammar that shouldn't
   * be shared just because they look the same. By performing reference
   * equality on the XML, we ensure that we only share parts of grammar that
   * are actually the same.
   */
  override def equals(that: Any): Boolean = {
    that match {
      case that: ShareKey => (this.xml eq that.xml) && (this.env == that.env)
      case _ => false
    }
  }
}

/**
 * Shared object factory/cache.
 *
 * Creates objects for new keys, shares previously computed ones (and avoids
 * recomputing them) for existing keys.
 */
final class SharedFactory[SharedType] {

  private val vals = new mutable.HashMap[ShareKey, SharedType]

  /**
   * The passing of the value argument by name is critical here, as
   * we want to avoid evaluating that at all when the key is one
   * we have already seen.
   */
  final def getShared(key: ShareKey, valueArg: => SharedType): SharedType = {
    lazy val value = valueArg // once only
    val opt = vals.get(key)
    opt match {
      case Some(y) => y
      case None => {
        val shared = value
        vals.put(key, shared)
        shared
      }
    }
  }
}

/**
 * Shared characteristics of any annotated schema component.
 *
 * Not all components can carry DFDL annotations.
 */
trait AnnotatedSchemaComponent
  extends SchemaComponent
  with AnnotatedMixin
  with OverlapCheckMixin {

  protected override def initialize(): Unit = {
    super.initialize()
  }

  /**
   * If two terms have the same propEnv, they have identical properties
   * in scope.
   */
  private lazy val propEnv: PropEnv = {
    val localPropsSets = this.nonDefaultFormatChain.propertyPairsSets
    val defaultPropObj = this.defaultFormatChain.propertyPairsSets
    val next = refersToForPropertyCombining
    PropEnv(localPropsSets, defaultPropObj, next.map { _.xml })
  }

  /**
   * The thing that provides the actual definition. If this is a local
   * element decl or local sequence/choice, then this is the thing,
   * if this is some sort of a ref, then the referenced definition is the thing.
   */
  private lazy val actualDef: AnnotatedSchemaComponent = {
    this match {
      case gr: GroupRef => gr.groupDef
      case aer: AbstractElementRef => aer.referencedElement
      case eb: ElementBase => eb
      case grl: GroupDefLike => grl
    }
  }

  protected final lazy val shareKey = ShareKey(actualDef.xml, propEnv)

  /**
   * For property combining only. E.g., doesn't refer from an element
   * to its complex type because we don't combine properties with that
   * in DFDL v1.0. (I consider that a language design bug in DFDL v1.0, but
   * that is the way it's defined.)
   */
  final protected def refersToForPropertyCombining: Option[AnnotatedSchemaComponent] =
    optReferredToComponent

  def optReferredToComponent: Option[AnnotatedSchemaComponent] // override in ref objects

  lazy val nonDefaultPropertySources: Seq[ChainPropProvider] = LV('nonDefaultPropertySources) {
    this match {
      case sd: SchemaDocument => Nil
      case _ => {
        val refTo = refersToForPropertyCombining
        val chainFromReferredTo = refTo
          .map { c =>
            val ndps = c.nonDefaultPropertySources
            ndps
          }
          .toSeq
          .flatten
        val myNDFC = nonDefaultFormatChain
        val completeNonDefaultFormatChain =
          myNDFC +: chainFromReferredTo
        val seq = completeNonDefaultFormatChain.distinct
        checkNonOverlap(seq)
        seq
      }
    }
  }.value

  final protected lazy val defaultPropertySources: Seq[ChainPropProvider] =
    LV('defaultPropertySources) {
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

  private def isDfdlNamespace(ns: String): Boolean = ns.contains("ogf") && ns.contains("dfdl")

  def getDFDLAppinfos(appinfos: NodeSeq): NodeSeq = {
    val dais = appinfos.filter { ai =>
      {
        ai.attribute("source") match {
          case None => {
            // if a child node in the dfdl namespace exists we will provide a warning about using the source property
            ai.child.flatMap(n => Option(n.namespace)).find(isDfdlNamespace).foreach { _ =>
              SDW(
                WarnID.AppinfoNoSource,
                """xs:appinfo without source attribute. Is source="http://www.ogf.org/dfdl/" missing?""",
              )
            }
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
            val isAcceptable = isDfdlNamespace(sourceNS.toString)
            schemaDefinitionWarningWhen(
              WarnID.AppinfoDFDLSourceWrong,
              !hasRightSource && isAcceptable,
              "The xs:appinfo source attribute value '%s' should be '%s'.",
              sourceNS,
              officialAppinfoSourceAttributeNS,
            )
            (hasRightSource || isAcceptable)
          }
        }
      }
    }
    dais
  }

  lazy val dfdlAppInfos = {
    val ais = (annotationNode \ "appinfo")
    getDFDLAppinfos(ais)
  }

  /**
   * The DFDL annotations on the component, as objects
   * that are subtypes of DFDLAnnotation.
   */
  final lazy val annotationObjs = {
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

  final lazy val formatAnnotationExpectedName =
    emptyFormatFactory.xml.asInstanceOf[scala.xml.Elem].label

  protected def isMyFormatAnnotation(a: DFDLAnnotation): Boolean

  final lazy val formatAnnotation = {
    val format = annotationObjs.collect {
      case fa: DFDLFormatAnnotation if isMyFormatAnnotation(fa) => fa
    }
    val res = format match {
      case Seq() =>
        emptyFormatFactory // does make things with the right namespace scopes attached!
      case Seq(fa) => fa
      case _ =>
        schemaDefinitionError("Only one format annotation is allowed at each annotation point.")
    }
    res
  }

}
