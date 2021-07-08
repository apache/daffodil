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

import scala.collection.immutable.ListMap
import scala.xml.Node

import org.apache.daffodil.api.WarnID
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.LookupLocation
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.NoNamespace
import org.apache.daffodil.xml.RefQName
import org.apache.daffodil.xml.XMLUtils

case class DeprecatedProperty(namespace: NS, property: String, replacement: String)

object DeprecatedProperty {

  private val deprecatedProperties: Seq[DeprecatedProperty] = Seq(
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerTransform", "dfdlx:layerTransform"),
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerEncoding", "dfdlx:layerEncoding"),
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerLengthKind", "dfdlx:layerLengthKind"),
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerLength", "dfdlx:layerLength"),
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerLengthUnits", "dfdlx:layerLengthUnits"),
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerBoundaryMark", "dfdlx:layerBoundaryMark"),
    DeprecatedProperty(XMLUtils.EXT_NS_APACHE, "parseUnparsePolicy", "dfdlx:parseUnparsePolicy"),
    DeprecatedProperty(XMLUtils.EXT_NS_NCSA, "parseUnparsePolicy", "dfdlx:parseUnparsePolicy"))

  def warnIfDeprecated(
    propertyName: String,
    propertyNS: NS,
    context: SchemaComponent): Unit = {

    val deprecation = deprecatedProperties.find { dp =>
      dp.property == propertyName && dp.namespace == propertyNS
    }

    if (deprecation.isDefined) {
      val warnID = propertyNS match {
        case XMLUtils.DFDL_NAMESPACE => WarnID.DeprecatedPropertyDFDLError
        case XMLUtils.DFDLX_NAMESPACE => WarnID.DeprecatedPropertyDFDLXError
        case XMLUtils.EXT_NS_APACHE => WarnID.DeprecatedPropertyDAFError
        case XMLUtils.EXT_NS_NCSA => WarnID.DeprecatedPropertyDAFError
        case _ => Assert.impossible()
      }

      context.SDW(warnID, "Property %s is deprecated. Use %s instead.", propertyName, deprecation.get.replacement)
    }
  }
}

/**
 * Base class for annotations that carry format properties
 */
abstract class DFDLFormatAnnotation(nodeArg: Node, annotatedSCArg: AnnotatedSchemaComponent)
  extends DFDLAnnotation(nodeArg, annotatedSCArg)
  with LeafPropProvider {

  //
  // prefix of a QName as the value of this ref, must be referring
  // to a namespace binding that is in force right here on this object
  // So we can resolve the QName relative to this.
  //
  private lazy val qns = ref.map { resolveQName(_) }
  private lazy val referencedDefineFormat = qns.flatMap { case qn => schemaSet.getDefineFormat(qn) }
  lazy val referencedFormat = referencedDefineFormat.map { _.formatAnnotation }

  /**
   * gets the dfdl:ref short form attribute, or if we have a long
   * form format annotation (or we ARE a long form format annotation)
   * gets the ref long form attribute.
   */
  final lazy val ref: Option[String] = {
    // We have to check if the ref exists in long form (dfdl:ref)
    // or short form (ref).
    //
    // longForm references are not prefixed by a dfdl namespace
    // they are actually located in the annotation located
    // on the node (DFDLAnnotation) itself.
    val optRefLongForm = getAttributeOption("ref")
    // Applicable shortForm ref has a dfdl namespace prefix and is located
    // on the actual Schema Component.
    val optRefShortForm = annotatedSC.getAttributeOption(XMLUtils.DFDL_NAMESPACE, "ref")
    (optRefLongForm, optRefShortForm) match {
      case (None, Some(s)) => Some(s)
      case (Some(s), None) => Some(s)
      case (Some(sh), Some(lg)) =>
        schemaDefinitionError("Both long form and short form ref attribute found.")
      case (None, None) => None
    }
  }

  private def adjustNamespace(ns: NS) = {
    ns match {
      case NoNamespace => annotatedSC.targetNamespace // this could also be NoNamespace, but that's ok.
      case _ => ns
    }
  }

  // The ListMap collection preserves insertion order.
  private type NamedFormatMap = ListMap[RefQName, DFDLFormat]

  private val emptyNamedFormatMap = ListMap[RefQName, DFDLFormat]()
  /**
   * build up map of what we have 'seen' as we go so we can detect cycles
   */
  protected def getFormatRefs(seen: NamedFormatMap): NamedFormatMap = {
    val res =
      qns.map {
        case qn =>
          // first we have to adjust the namespace
          // because a file with no target namespace,
          // can reference something in another file, which also has no target
          // namespace. The files can collectively or by nesting, be
          // included in a third file that has a namespace, and in that
          // case all the format definitions being created as those
          // files are loaded will be in that third namespace.
          // so just because we had <dfdl:format ref="someFormat"/> and the
          // ref has no namespace prefix on it, doesn't mean that the
          // defineFormat we're seeking is in no namespace.
          val adjustedNS = adjustNamespace(qn.namespace)
          val adjustedQN = RefQName(None, qn.local, adjustedNS)
          val notSeenIt = seen.get(adjustedQN) == None
          schemaDefinitionUnless(notSeenIt, "Format ref attributes form a cycle: \n%s\n%s",
            (adjustedQN, locationDescription),
            seen.map { case (qn, fmtAnn) => (qn, fmtAnn.locationDescription) }.mkString("\n"))
          val defFmt = schemaSet.getDefineFormat(adjustedQN).getOrElse {
            annotatedSC.schemaDefinitionError("defineFormat with name '%s', was not found.", adjustedQN.toString)
          }
          val fmt = defFmt.formatAnnotation
          val newSeen = seen + (adjustedQN -> fmt)
          val moreRefs = fmt.getFormatRefs(newSeen)
          moreRefs
      }.getOrElse({
        lazy val seenStrings = seen.map {
          case (qn, v) => qn.local // + " is " + v.xml
        }.toSeq
        seen
      })
    res
  }

  /**
   * A flat map where each entry is (ns, ln) onto DFDLFormatAnnotation.
   */
  private lazy val formatRefMap = getFormatRefs(emptyNamedFormatMap)

  lazy val formatChain: ChainPropProvider = {
    val formatAnnotations = formatRefMap.map { case (_, fa) => fa }.toSeq
    val withMe = (this +: formatAnnotations).distinct
    val res = new ChainPropProvider(withMe, this.diagnosticDebugName)
    res
  }

  private lazy val shortFormProperties: Set[PropItem] = LV[Set[PropItem]]('shortFormProperties) {
    // shortForm properties should be prefixed by dfdl
    // Remove the dfdl prefix from the attributes so that they
    // can be properly combined later.
    val dfdlKvPairs = XMLUtils.dfdlAttributes(annotatedSC.xml).asAttrMap.map {
      case (key: String, value: String) => (removePrefix(key), value)
    }
    val dfdlxKvPairs = XMLUtils.dfdlxAttributes(annotatedSC.xml).asAttrMap.map {
      case (key: String, value: String) => (removePrefix(key), value)
    }
    val dafKvPairs = XMLUtils.dafAttributes(annotatedSC.xml).asAttrMap.map {
      case (key: String, value: String) => (removePrefix(key), value)
    }
    dfdlKvPairs.keys.foreach { propName =>
      DeprecatedProperty.warnIfDeprecated(propName, XMLUtils.DFDL_NAMESPACE, this)
    }
    dfdlxKvPairs.keys.foreach { propName =>
      DeprecatedProperty.warnIfDeprecated(propName, XMLUtils.DFDLX_NAMESPACE, this)
    }
    dafKvPairs.keys.foreach { propName =>
      DeprecatedProperty.warnIfDeprecated(propName, XMLUtils.EXT_NS_APACHE, this)
    }

    val kvPairs = dfdlKvPairs ++ dfdlxKvPairs ++ dafKvPairs
    val kvPairsButNotRef = kvPairs.filterNot { _._1 == "ref" } // dfdl:ref is NOT a property
    val pairs = kvPairsButNotRef.map { case (k, v) => (k, (v, annotatedSC)).asInstanceOf[PropItem] }
    pairs.toSet
  }.value

  private lazy val longFormProperties: Set[PropItem] = LV[Set[PropItem]]('longFormProperties) {
    // longForm Properties are not prefixed by dfdl
    val dfdlAttrs = dfdlAttributes(xml).asAttrMap
    schemaDefinitionUnless(dfdlAttrs.isEmpty, "long form properties are not prefixed by dfdl:")
    // however, extension properties are prefixed, even in long form
    val dfdlxAttrMap = dfdlxAttributes(xml).asAttrMap.map {
      case (key: String, value: String) => (removePrefix(key), value)
    }
    val dafAttrMap = dafAttributes(xml).asAttrMap.map {
      case (key: String, value: String) => (removePrefix(key), value)
    }
    //
    // TODO: This strips away any qualified attribute
    // That won't work when we add extension attributes
    // like daffodil:asAttribute="true"
    //
    val kvPairs = xml.attributes.asAttrMap.collect {
      case (k, v) if (!k.contains(":")) => (k, v)
    }
    val unqualifiedAttribs = kvPairs.filterNot { _._1 == "ref" } // get the ref off there. it is not a property.

    unqualifiedAttribs.keys.foreach { propName =>
      DeprecatedProperty.warnIfDeprecated(propName, XMLUtils.DFDL_NAMESPACE, this)
    }
    dfdlxAttrMap.keys.foreach { propName =>
      DeprecatedProperty.warnIfDeprecated(propName, XMLUtils.DFDLX_NAMESPACE, this)
    }
    dafAttrMap.keys.foreach { propName =>
      DeprecatedProperty.warnIfDeprecated(propName, XMLUtils.EXT_NS_APACHE, this)
    }

    val dfdlAndExtAttribs = unqualifiedAttribs ++ dfdlxAttrMap ++ dafAttrMap
    val res = dfdlAndExtAttribs.map { case (k, v) => (k, (v, this.asInstanceOf[LookupLocation])) }.toSet
    res
  }.value

  private lazy val elementFormPropertyAnnotations = {
    val props = xml \\ "property"
    val res = props.map {
      new DFDLProperty(_, this)
    }
    res
  }

  private lazy val elementFormProperties: Set[PropItem] = LV[Set[PropItem]]('elementFormProperties) {
    elementFormPropertyAnnotations.foreach { p =>
      DeprecatedProperty.warnIfDeprecated(p.name, p.propertyNamespace, p)
    }

    elementFormPropertyAnnotations.map { p => (p.name, (p.value, p)) }.toSet
  }.value

  /**
   * 'locallyConflicting' means conflicting between the short form and long form and
   * element form properties that appear on this same format annotation
   * object locally. Not across references or schema components.
   */
  private lazy val locallyConflictingProperties = {
    val sf = shortFormProperties.map { case (n, _) => n }
    val lf = longFormProperties.map { case (n, _) => n }
    val ef = elementFormProperties.map { case (n, _) => n }
    val res = sf.intersect(lf).union(
      sf.intersect(ef)).union(
        lf.intersect(ef))
    res
  }

  private lazy val hasConflictingPropertyError = locallyConflictingProperties.size != 0

  private lazy val combinedJustThisOneProperties: PropMap = LV('combinedJustThisOneOproperties) {
    // We need this error to occur immediately! Didn't seem to be checked otherwise.
    schemaDefinitionUnless(
      !hasConflictingPropertyError,
      "Short, long, and element form properties overlap: %s at %s",
      locallyConflictingProperties.mkString(", "),
      this.locationDescription)
    val jtoSet = shortFormProperties.union(longFormProperties).union(elementFormProperties)
    val jto = jtoSet.toMap
    jto
  }.toOption.getOrElse(emptyPropMap)

  /**
   * Just this one, as in the short, long, and element form properties, on just this
   * annotated schema component, not following any ref chains. Just the properties
   * right here.
   *
   * Needed for certain warnings, and also is the primitive from which the
   * ChainPropProvider is built up. That one DOES follow ref chains.
   */
  final lazy val justThisOneProperties: PropMap = LV('justThisOneProperties) {
    val res = combinedJustThisOneProperties
    res
  }.toOption.getOrElse(emptyPropMap)

  /**
   * For unit testing convenience, or for use when debugging.
   */
  def getPropertyForUnitTest(propName: String) =
    justThisOneProperties.get(propName).get._1

  /**
   * For unit testing convenience, or for use when debugging.
   */
  def verifyPropValue(propName: String, expectedValue: String): Boolean = {
    val info = justThisOneProperties.get(propName)
    info match {
      case None => false
      case Some((actualValue, _)) => actualValue == expectedValue
    }
  }
}
