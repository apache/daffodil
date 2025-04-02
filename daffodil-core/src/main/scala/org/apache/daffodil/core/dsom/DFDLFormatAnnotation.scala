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

import scala.collection.immutable.ListMap
import scala.xml.Node

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.schema.annotation.props.LookupLocation
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.RefQName
import org.apache.daffodil.lib.xml.XMLUtils

case class DeprecatedProperty(namespace: NS, property: String, replacement: String)

object DeprecatedProperty {

  private def deprecatedProperties: Seq[DeprecatedProperty] = Seq(
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerTransform", "dfdlx:layer"),
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerEncoding", "dfdlx:layerEncoding"),
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerLengthKind", "dfdlx:layerLengthKind"),
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerLength", "dfdlx:layerLength"),
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerLengthUnits", "dfdlx:layerLengthUnits"),
    DeprecatedProperty(XMLUtils.DFDL_NAMESPACE, "layerBoundaryMark", "dfdlx:layerBoundaryMark"),
    DeprecatedProperty(
      XMLUtils.DFDLX_NAMESPACE,
      "emptyElementParsePolicy",
      "dfdl:emptyElementParsePolicy"
    ),
    DeprecatedProperty(
      XMLUtils.EXT_NS_APACHE,
      "parseUnparsePolicy",
      "dfdlx:parseUnparsePolicy"
    ),
    DeprecatedProperty(XMLUtils.EXT_NS_NCSA, "parseUnparsePolicy", "dfdlx:parseUnparsePolicy")
  )

  // introduced to eliminate linear search per property of the above list, which is expected
  // to fail every time, so every property lookup scans the whole thing.
  private lazy val deprecatedPropertyMap = deprecatedProperties.map { dp =>
    ((dp.property, dp.namespace), dp)
  }.toMap

  def warnIfDeprecated(propertyName: String, propertyNS: NS, context: SchemaComponent): Unit = {

    val deprecation = deprecatedPropertyMap.get((propertyName, propertyNS))

    if (deprecation.isDefined) {
      val warnID = propertyNS match {
        case XMLUtils.DFDL_NAMESPACE => WarnID.DeprecatedPropertyDFDLError
        case XMLUtils.DFDLX_NAMESPACE => WarnID.DeprecatedPropertyDFDLXError
        case XMLUtils.EXT_NS_APACHE => WarnID.DeprecatedPropertyDAFError
        case XMLUtils.EXT_NS_NCSA => WarnID.DeprecatedPropertyDAFError
        case _ => Assert.impossible()
      }

      context.SDW(
        warnID,
        "Property %s is deprecated. Use %s instead.",
        propertyName,
        deprecation.get.replacement
      )
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
  private lazy val referencedDefineFormat = qns.flatMap { case qn =>
    schemaSet.getDefineFormat(qn)
  }
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

  // The ListMap collection preserves insertion order.
  private type NamedFormatMap = ListMap[RefQName, DFDLFormat]

  private val emptyNamedFormatMap = ListMap[RefQName, DFDLFormat]()

  /**
   * build up map of what we have 'seen' as we go so we can detect cycles
   */
  protected def getFormatRefs(seen: NamedFormatMap): NamedFormatMap = {
    val res =
      qns
        .map { case qn =>
          val notSeenIt = seen.get(qn) == None
          schemaDefinitionUnless(
            notSeenIt,
            "Format ref attributes form a cycle: \n%s\n%s",
            (qn, locationDescription),
            seen.map { case (qn, fmtAnn) => (qn, fmtAnn.locationDescription) }.mkString("\n")
          )
          val defFmt = schemaSet.getDefineFormat(qn).getOrElse {
            annotatedSC.schemaDefinitionError(
              "defineFormat with name '%s', was not found.",
              qn.toString
            )
          }
          val fmt = defFmt.formatAnnotation
          val newSeen = seen + (qn -> fmt)
          val moreRefs = fmt.getFormatRefs(newSeen)
          moreRefs
        }
        .getOrElse({
          lazy val seenStrings = seen.map { case (qn, v) =>
            qn.local // + " is " + v.xml
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

  /**
   * Some properties (e.g. dfdl:ref, daf:suppressSchemaDefinitionWarnings) do not follow normal
   * scoping rules and so are manually handled elsewhere in Daffodil. These are the lists of
   * those properties used to exlucde them from property lookups.
   */
  private lazy val nonStandardDfdlProperties = Seq("ref")
  private lazy val nonStandardDfdlxProperties = Seq()
  private lazy val nonStandardDafProperties = Seq("suppressSchemaDefinitionWarnings")

  private lazy val shortFormProperties: Set[PropItem] =
    LV[Set[PropItem]](Symbol("shortFormProperties")) {
      // shortForm properties should be prefixed by dfdl
      // Remove the dfdl prefix from the attributes so that they
      // can be properly combined later.
      val dfdlKvPairs = XMLUtils
        .dfdlAttributes(annotatedSC.xml)
        .asAttrMap
        .map { kv => (removePrefix(kv._1), kv._2) }
        .filterNot { kv => nonStandardDfdlProperties.contains(kv._1) }
      val dfdlxKvPairs = XMLUtils
        .dfdlxAttributes(annotatedSC.xml)
        .asAttrMap
        .map { kv => (removePrefix(kv._1), kv._2) }
        .filterNot { kv => nonStandardDfdlxProperties.contains(kv._1) }
      val dafKvPairs = XMLUtils
        .dafAttributes(annotatedSC.xml)
        .asAttrMap
        .map { kv => (removePrefix(kv._1), kv._2) }
        .filterNot { kv => nonStandardDafProperties.contains(kv._1) }

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
      val pairs = kvPairs.map { case (k, v) =>
        (k, (v, annotatedSC)).asInstanceOf[PropItem]
      }
      pairs.toSet
    }.value

  private lazy val longFormProperties: Set[PropItem] =
    LV[Set[PropItem]](Symbol("longFormProperties")) {
      // longForm Properties are not prefixed by dfdl
      schemaDefinitionUnless(
        dfdlAttributes(xml).isEmpty,
        "long form properties are not prefixed by dfdl:"
      )
      val dfdlAttrMap = xml.attributes.asAttrMap
        .filter { kv => !kv._1.contains(":") }
        .filterNot { kv => nonStandardDfdlProperties.contains(kv._1) }
      // however, extension properties are prefixed, even in long form
      val dfdlxAttrMap = dfdlxAttributes(xml).asAttrMap
        .map { kv => (removePrefix(kv._1), kv._2) }
        .filterNot { kv => nonStandardDfdlxProperties.contains(kv._1) }
      val dafAttrMap = dafAttributes(xml).asAttrMap
        .map { kv => (removePrefix(kv._1), kv._2) }
        .filterNot { kv => nonStandardDafProperties.contains(kv._1) }

      dfdlAttrMap.keys.foreach { propName =>
        DeprecatedProperty.warnIfDeprecated(propName, XMLUtils.DFDL_NAMESPACE, this)
      }
      dfdlxAttrMap.keys.foreach { propName =>
        DeprecatedProperty.warnIfDeprecated(propName, XMLUtils.DFDLX_NAMESPACE, this)
      }
      dafAttrMap.keys.foreach { propName =>
        DeprecatedProperty.warnIfDeprecated(propName, XMLUtils.EXT_NS_APACHE, this)
      }

      val dfdlAndExtAttribs = dfdlAttrMap ++ dfdlxAttrMap ++ dafAttrMap
      val res = dfdlAndExtAttribs.map { case (k, v) =>
        (k, (v, this.asInstanceOf[LookupLocation]))
      }.toSet
      res
    }.value

  private lazy val elementFormPropertyAnnotations = {
    val props = xml \\ "property"
    val res = props.map {
      new DFDLProperty(_, this)
    }
    res
  }

  private lazy val elementFormProperties: Set[PropItem] =
    LV[Set[PropItem]](Symbol("elementFormProperties")) {
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
    val res = sf.intersect(lf).union(sf.intersect(ef)).union(lf.intersect(ef))
    res
  }

  private lazy val hasConflictingPropertyError = locallyConflictingProperties.size != 0

  private lazy val combinedJustThisOneProperties: PropMap =
    LV(Symbol("combinedJustThisOneOproperties")) {
      // We need this error to occur immediately! Didn't seem to be checked otherwise.
      schemaDefinitionUnless(
        !hasConflictingPropertyError,
        "Short, long, and element form properties overlap: %s at %s",
        locallyConflictingProperties.mkString(", "),
        this.locationDescription
      )
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
  final lazy val justThisOneProperties: PropMap = LV(Symbol("justThisOneProperties")) {
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
