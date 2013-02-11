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


import scala.xml._
import scala.xml.parsing._
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.xml._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropertyMixin
import edu.illinois.ncsa.daffodil.util.Info
import edu.illinois.ncsa.daffodil.util.Debug
import scala.collection.immutable.ListMap

/**
 * Base class for any DFDL annotation
 */
abstract class DFDLAnnotation(xmlArg: Node, annotatedSCArg: AnnotatedSchemaComponent)
  extends DiagnosticsProviding
  with ImplementsThrowsSDE
  with LookupLocation
  with FindPropertyMixin {

  // delegate to the annotated component.
  def findPropertyOption(pname: String): PropertyLookupResult = {
    val res = annotatedSC.findPropertyOption(pname)
    res
  }

  val annotatedSC = annotatedSCArg
  val xml = xmlArg
  lazy val schemaSet = annotatedSC.schemaSet
  lazy val schemaDocument = annotatedSC.schemaDocument
  lazy val schemaComponent = annotatedSC

  override lazy val fileName = annotatedSC.fileName

  override def addDiagnostic(diag: Diagnostic) = annotatedSC.addDiagnostic(diag)

  lazy val context = annotatedSC
  //  match {
  //      case sc : SchemaComponent => sc 
  //      case _ => Assert.invariantFailed("should be a SchemaComponent")
  //    }

  lazy val prettyName = xml.prefix + ":" + xml.label
  lazy val path = annotatedSC.path + "::" + prettyName

  override def toString() = path
}

trait RawCommonRuntimeValuedPropertiesMixin
  extends PropertyMixin {

  lazy val byteOrderRaw = {
    val s = getProperty("byteOrder")
    val res = s match {
      case "bigEndian" => """{ "bigEndian" }"""
      case "littleEndian" => """{ "littleEndian" }"""
      case a => a
    }
    res
  }

  lazy val encodingRaw = getProperty("encoding")
  lazy val outputNewLineRaw = getProperty("outputNewLine")
}

trait RawDelimitedRuntimeValuedPropertiesMixin
  extends RawCommonRuntimeValuedPropertiesMixin {

  lazy val initiatorRaw = getProperty("initiator")
  lazy val terminatorRaw = getProperty("terminator")
}

trait RawElementRuntimeValuedPropertiesMixin
  extends RawDelimitedRuntimeValuedPropertiesMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin {

  lazy val lengthRaw = getProperty("length")
  lazy val occursCountRaw = getProperty("occursCount")
}

trait RawSequenceRuntimeValuedPropertiesMixin
  extends RawDelimitedRuntimeValuedPropertiesMixin {

  lazy val separatorRaw = getProperty("separator")
}

trait RawEscapeSchemeRuntimeValuedPropertiesMixin
  extends PropertyMixin {

  //lazy val escapeKind = getProperty("escapeKind")
  //  lazy val escapeSchemeObject = {
  //
  //  }

  lazy val escapeCharacterRaw = getProperty("escapeCharacter")
  lazy val escapeEscapeCharacterRaw = getProperty("escapeEscapeCharacter")
  //  lazy val escapeBlockStartRaw = getProperty("escapeBlockStart")
  //  lazy val escapeBlockEndRaw = getProperty("escapeBlockEnd")
}

trait RawSimpleTypeRuntimeValuedPropertiesMixin
  extends RawCommonRuntimeValuedPropertiesMixin {

  def textStandardDecimalSeparatorRaw = getProperty("textStandardDecimalSeparator")
  def textStandardGroupingSeparatorRaw = getProperty("textStandardGroupingSeparator")
  // TODO: update when textStandardExponentCharacter is phased out.
  def textStandardExponentRepRaw = getProperty("textStandardExponentRep") // Note: name changed to suffix of "...Rep" via Errata
  def binaryFloatRepRaw = getProperty("binaryFloatRep")
  def textBooleanTrueRepRaw = getProperty("textBooleanTrueRep")
  def textBooleanFalseRepRaw = getProperty("textBooleanFalseRep")

}

class DFDLProperty(val xml: Node, formatAnnotation: DFDLFormatAnnotation)
  extends LookupLocation
  with ImplementsThrowsSDE {

  lazy val prettyName = xml.prefix + ":" + xml.label
  lazy val path = formatAnnotation.path + "::" + prettyName

  lazy val schemaComponent = formatAnnotation.annotatedSC
  lazy val context = schemaComponent

  lazy val schemaDocument = formatAnnotation.schemaDocument
  lazy val fileName = schemaDocument.fileName

  lazy val <dfdl:property>{ valueNodes }</dfdl:property> = xml

  // TODO: if we grab the value from here, then any qnames inside that value
  // have to be resolved by THIS Object
  lazy val value = valueNodes.text

  lazy val name = getAttributeRequired("name")

  lazy val diagnosticChildren: DiagnosticsList = Nil
}

/**
 * Base class for annotations that carry format properties
 */
abstract class DFDLFormatAnnotation(nodeArg: Node, annotatedSCArg: AnnotatedSchemaComponent)
  extends DFDLAnnotation(nodeArg, annotatedSCArg)
  //  with RawCommonRuntimeValuedPropertiesMixin
  //  with RawEscapeSchemeRuntimeValuedPropertiesMixin
  with LeafPropProvider {

  lazy val diagnosticChildren: DiagnosticsList = Nil

  lazy val ref = getLocalFormatRef()
  lazy val refPair = ref.map { resolveQName(_) }
  lazy val referencedDefineFormat = refPair.flatMap { case (ns, name) => schemaSet.getDefineFormat(ns, name) }
  lazy val referencedFormat = referencedDefineFormat.map { _.formatAnnotation }

  /**
   * gets the dfdl:ref short form attribute, or if we have a long
   * form format annotation (or we ARE a long form format annotation)
   * gets the ref long form attribute.
   */
  private def getLocalFormatRef(): Option[String] = {
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

  def adjustNamespace(ns: NS) = {
    ns match {
      case NoNamespace => annotatedSC.targetNamespace // this could also be NoNamespace, but that's ok.
      case _ => ns
    }
  }

  // The ListMap collection preserves insertion order.
  type NamedFormatMap = ListMap[(NS, String), DFDLFormat]

  val emptyNamedFormatMap = ListMap[(NS, String), DFDLFormat]()
  /**
   * build up map of what we have 'seen' as we go so we can detect cycles
   */
  private def getFormatRefs(seen: NamedFormatMap): NamedFormatMap = {
    val res =
      refPair.map {
        case pair @ (ns, ln) =>
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
          val adjustedNS = adjustNamespace(ns)
          val newPair = (adjustedNS, ln)
          val notSeenIt = seen.get(newPair) == None
          schemaDefinition(notSeenIt, "Format ref attributes form a cycle: \n%s\n%s",
            (newPair, locationDescription),
            seen.map { case (pair, fmtAnn) => (pair, fmtAnn.locationDescription) }.mkString("\n"))
          val defFmt = schemaSet.getDefineFormat(adjustedNS, ln).getOrElse(
            schemaDefinitionError("defineFormat with name %s, was not found.", newPair))
          log(Debug("found defineFormat named: %s", newPair))
          val fmt = defFmt.formatAnnotation
          val newSeen = seen + (newPair -> fmt)
          // println("seen now: " + newSeen)
          val moreRefs = fmt.getFormatRefs(newSeen)
          // println("final seen: " + moreRefs)
          moreRefs
      }.getOrElse({
        lazy val seenStrings = seen.map {
          case ((ns, name), v) => name // + " is " + v.xml 
        }.toSeq
        log(Debug("Property sources are: %s", seenStrings.mkString("\n")))
        seen
      })
    res
  }

  /**
   * A flat map where each entry is (ns, ln) onto DFDLFormatAnnotation.
   */
  final lazy val formatRefMap = getFormatRefs(emptyNamedFormatMap)

  def getFormatChain(): ChainPropProvider = {
    val formatAnnotations = formatRefMap.map { case ((_, _), fa) => fa }.toSeq
    val withMe = (this +: formatAnnotations).distinct
    val res = new ChainPropProvider(withMe, this)
    res
  }

  /**
   * Don't need the map anymore, and we put ourselves highest
   * priority meaning at the front of the list.
   */
  lazy val formatRefs: Seq[DFDLFormatAnnotation] = {
    val fmts = formatRefMap.map { case ((ns, ln), fmt) => fmt }
    log(Debug("%s::%s formatRefs = %s", annotatedSC.prettyName, prettyName, fmts))
    val seq = Seq(this) ++ fmts
    seq
  }

  lazy val shortFormProperties: Set[PropItem] = {
    // shortForm properties should be prefixed by dfdl
    // Remove the dfdl prefix from the attributes so that they
    // can be properly combined later.
    val kvPairs = XMLUtils.dfdlAttributes(annotatedSC.xml).asAttrMap.map {
      case (key: String, value: String) => (removePrefix(key), value)
    }
    val kvPairsButNotRef = kvPairs.filterNot { _._1 == "ref" } // dfdl:ref is NOT a property
    val pairs = kvPairsButNotRef.map { case (k, v) => (k, (v, annotatedSC)).asInstanceOf[PropItem] }
    pairs.toSet
  }

  lazy val longFormProperties: Set[PropItem] = {
    // longForm Properties are not prefixed by dfdl
    val dfdlAttrs = dfdlAttributes(xml).asAttrMap
    schemaDefinition(dfdlAttrs.isEmpty, "long form properties are not prefixed by dfdl:")
    //
    // TODO: This strips away any qualified attribute
    // That won't work when we add extension attributes 
    // like daffodil:asAttribute="true"
    //
    val kvPairs = xml.attributes.asAttrMap.collect {
      case (k, v) if (!k.contains(":")) => (k, v)
    }
    val unqualifiedAttribs = kvPairs.filterNot { _._1 == "ref" } // get the ref off there. it is not a property.
    val res = unqualifiedAttribs.map { case (k, v) => (k, (v, this.asInstanceOf[LookupLocation])) }.toSet
    res
  }

  private lazy val elementFormPropertyAnnotations =
    (xml \\ "property").map { new DFDLProperty(_, this) }

  lazy val elementFormProperties: Set[PropItem] = {
    elementFormPropertyAnnotations.map { p => (p.name, (p.value, p)) }.toSet
  }

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

  private lazy val combinedJustThisOneProperties: PropMap = {
    // We need this error to occur immediately! Didn't seem to be checked otherwise.
    schemaDefinition(!hasConflictingPropertyError,
      "Short, long, and element form properties overlap: %s at %s",
      locallyConflictingProperties.mkString(", "),
      this.locationDescription)
    // jto = "just this one"
    val jtoSet = shortFormProperties.union(longFormProperties).union(elementFormProperties)
    val jto = jtoSet.toMap
    jto
  }

  /**
   * Just this one, as in the short, long, and element form properties, on just this
   * annotated schema component, not following any ref chains. Just the properties
   * right here.
   *
   * Needed for certain warnings, and also is the primitive from which the
   * ChainPropProvider is built up. That one DOES follow ref chains.
   */
  override lazy val justThisOneProperties: PropMap = justThisOneProperties_.value
  private lazy val justThisOneProperties_ = LV('justThisOneProperties) {
    val res = combinedJustThisOneProperties
    log(Debug("%s::%s justThisOneProperties are: %s", annotatedSC.prettyName, prettyName, res))
    res
  }

}

/**
 * Base class for assertions, variable assignments, etc
 */
abstract class DFDLStatement(node: Node, annotatedSC: AnnotatedSchemaComponent)
  extends DFDLAnnotation(node, annotatedSC) {

  lazy val diagnosticChildren: DiagnosticsList = List(gram)
  def gram: Gram
}

class DFDLFormat(node: Node, sd: SchemaDocument)
  extends DFDLFormatAnnotation(node, sd)
// leave the below comments here for a while. (In case we have to reproduce
// this list of mixins on a schema component somewhere)
//  with Format_AnnotationMixin
//  with NillableMixin
//  with SeparatorSuppressionPolicyMixin
//  with RawElementRuntimeValuedPropertiesMixin
//  with RawSequenceRuntimeValuedPropertiesMixin

abstract class DFDLNonDefaultFormatAnnotation(node: Node, sc: AnnotatedSchemaComponent)
  extends DFDLFormatAnnotation(node, sc)

class DFDLElement(node: Node, decl: ElementBase)
  extends DFDLNonDefaultFormatAnnotation(node, decl)

class DFDLGroup(node: Node, decl: GroupBase)
  extends DFDLNonDefaultFormatAnnotation(node, decl)

abstract class DFDLModelGroup(node: Node, decl: ModelGroup)
  extends DFDLNonDefaultFormatAnnotation(node, decl)

class DFDLSequence(node: Node, decl: Sequence)
  extends DFDLModelGroup(node, decl)

class DFDLChoice(node: Node, decl: Choice)
  extends DFDLModelGroup(node, decl)

class DFDLSimpleType(node: Node, decl: SimpleTypeDefBase)
  extends DFDLNonDefaultFormatAnnotation(node, decl)
// Leave the below comments in place. These are not reproduced (currently)
// on SimpleTypeDefBase. It appears these properties are only accessed
// indirectly by way of an ElementDecl that uses this type.
//
//  with SimpleType_AnnotationMixin
//  with TextNumberFormatMixin
//  with StringTextMixin
//  with NumberTextMixin
//  with CalendarTextMixin
//  with BooleanTextMixin
//  with RawSimpleTypeRuntimeValuedPropertiesMixin 

trait DFDLDefiningAnnotation
  extends NamedAnnotationAndComponentMixin { self: DFDLAnnotation =>

  // The point of this, is so we can match-case on type DFDLDefiningAnnotation
  // but then still conveniently use methods/members defined in the 
  // DFDLAnnotation class
  val asAnnotation = self

  lazy val nom = getAttributeRequired("name") // validation will check this for us.
  override lazy val prettyName: String = nom // note: name is not a format property.
  override lazy val name: String = nom

  lazy val expandedNCNameToQName = XMLUtils.expandedQName(context.targetNamespace, nom)

  lazy val definingAnnotationDiagnosticChildren: DiagnosticsList = Nil
}

class DFDLDefineFormat(node: Node, sd: SchemaDocument)
  extends DFDLAnnotation(node, sd) // Note: DefineFormat is not a format annotation
  with DFDLDefiningAnnotation // with DefineFormat_AnnotationMixin // mixins are only for format annotations.
  {

  lazy val baseFormat = getAttributeOption("baseFormat") // nor baseFormat

  lazy val formatAnnotation = formatAnnotation_.value
  private lazy val formatAnnotation_ = LV('formatAnnotation) {
    Utility.trim(node) match {
      case <dfdl:defineFormat>{ f @ <dfdl:format>{ contents @ _* }</dfdl:format> }</dfdl:defineFormat> =>
        new DFDLFormat(f, sd)
      case _ => Assert.impossibleCase()
    }
  }

  lazy val diagnosticChildren: DiagnosticsList = formatAnnotation +: definingAnnotationDiagnosticChildren
}

class DFDLEscapeScheme(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLFormatAnnotation(node, decl)
  with EscapeScheme_AnnotationMixin
  with RawEscapeSchemeRuntimeValuedPropertiesMixin {

  override def findPropertyOption(pname: String): PropertyLookupResult = {
    val propNodeSeq = xml.attribute(pname)
    propNodeSeq match {
      case None => NotFound(Seq(this), Nil) // attribute was not found
      case Some(nodeseq) => {
        //
        // Interesting that attributeName="" produces a Nil nodeseq, not an empty string.
        // 
        // This whole attributes as NodeSeq thing in Scala seems strange, but attributes
        // can contain unresolved entities, e.g., quote="&amp;quot;2B || ! 2B&amp;quot;"
        // so really they do have to return them as node sequences. It requires DTD processing
        // to resolve everything, and most code isn't going to process the DTDs. I.e., the scala 
        // XML library lets your code be the one doing the DTD resolving, so they can't do it for you.
        //
        nodeseq match {
          case Nil => Found("", this) // we want to hand back the empty string as a value.
          case _ => Found(nodeseq.toString, this)
        }
      }
    }
  }
}

class DFDLDefineEscapeScheme(node: Node, decl: SchemaDocument)
  extends DFDLAnnotation(node, decl) // Note: defineEscapeScheme isn't a format annotation itself.
  with DFDLDefiningAnnotation // with DefineEscapeScheme_AnnotationMixin 
  {

  lazy val escapeScheme = Utility.trim(node) match {
    case <dfdl:defineEscapeScheme>{ e @ <dfdl:escapeScheme>{ contents @ _* }</dfdl:escapeScheme> }</dfdl:defineEscapeScheme> =>
      new DFDLEscapeScheme(e, decl)
    case _ => Assert.impossibleCase()
  }

  lazy val diagnosticChildren: DiagnosticsList = escapeScheme +: definingAnnotationDiagnosticChildren

  override def toString(): String = {
    "DFDLDefineEscapeScheme." + name
  }
}

abstract class DFDLAssertionBase(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl) {
  private lazy val testAttrib = getAttributeOption("test")

  // tolerate whitespace. E.g., 
  //   <assert test="...">
  //   </assert>
  // on two lines, indented, has whitespace in the body of the element.
  // Just reformatting XML, or printing it out with a pretty printer
  // can break assertions otherwise.
  //
  // even if you write <assert><![CDATA[{ ... }]]></assert> 
  // you can still lose because the implementation might convert
  // the schema to a string (by pretty printing), and this may re-insert 
  // whitespace.
  //
  // So, we trim the body string.
  private[dsom] lazy val testBody: Option[String] = node.child.text match {
    case s if (s.trim().length() == 0) => None
    case txt => Some(txt.trim())
  } // package visible for unit testing

  private lazy val testPattern = getAttributeOption("testPattern")
  lazy val testKind = getAttributeOption("testKind") match {
    case Some(str) => TestKind(str, decl)
    case None => TestKind.Expression
  }

  private lazy val messageAttrib = getAttributeOption("message")
  lazy val message = messageAttrib match {
    case None => "%s failed".format(testTxt)
    case Some(s) => s
  }

  lazy val testTxt = (testKind, testBody, testAttrib, testPattern) match {
    case (TestKind.Expression, None, Some(txt), None) => txt
    case (TestKind.Expression, txt, None, None) => txt.get
    case (TestKind.Pattern, None, None, pat) => pat.get
    case (TestKind.Expression, Some(bdy), Some(attrib), _) => SDE("You may not specify both test attribute and a body expression.")
    case (TestKind.Expression, None, None, _) => SDE("You must specify either a test attribute or a body expression.")
    case (TestKind.Pattern, Some(bdy), _, Some(txt)) => SDE("You may not specify both testPattern attribute and a body expression.")
    case (TestKind.Pattern, None, _, None) => SDE("You must specify either a testPattern attribute or a body expression. for testKind='pattern'")
    case (TestKind.Pattern, Some(bdy), None, None) => bdy // pattern as body of assert element
    case (TestKind.Pattern, _, Some(tst), _) => SDE("You cannot specify test='%s' for testKind='pattern'", tst)
    case (TestKind.Expression, _, _, Some(pat)) => SDE("You cannot specify testPattern='%s' for testKind='expression' (which is the default test kind.)", pat)
    case _ => Assert.invariantFailed("unexpected case.")
  }

  //
  // TODO: override diagnosticChildren if we compile the testBody/pattern into an object
  // which can provide error/diagnostic information itself (beyond what this class itself
  // can provide... which is nothing right now, but it could...someday).
  //
}

class DFDLAssert(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLAssertionBase(node, decl) { // with Assert_AnnotationMixin // Note: don't use these generated mixins. Statements don't have format properties
  lazy val gram = gram_.value
  private lazy val gram_ = LV('gram) {
    testKind match {
      case TestKind.Pattern => AssertPatternPrim(decl, this)
      case TestKind.Expression => AssertBooleanPrim(decl, this)
    }
  }
}

class DFDLDiscriminator(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLAssertionBase(node, decl) { // with Discriminator_AnnotationMixin 
  lazy val gram = gram_.value
  private lazy val gram_ = LV('gram) {
    testKind match {
      case TestKind.Pattern => DiscriminatorPatternPrim(decl, this)
      case TestKind.Expression => DiscriminatorBooleanPrim(decl, this)
    }
  }
}

class DFDLDefineVariable(node: Node, doc: SchemaDocument)
  extends DFDLStatement(node, doc)
  with DFDLDefiningAnnotation {
  lazy val gram = EmptyGram // has to have because statements have parsers layed in by the grammar.
  lazy val typeQName = getAttributeOption("type").getOrElse("xs:string")
  lazy val external = getAttributeOption("external").map { _.toBoolean }.getOrElse(false)
  lazy val defaultValueAsAttribute = getAttributeOption("defaultValue")
  lazy val defaultValueAsElement = node.child.text.trim
  lazy val defaultValue = (defaultValueAsAttribute, defaultValueAsElement) match {
    case (None, "") => None
    case (None, str) => Some(str)
    case (Some(str), "") => Some(str)
    case (Some(str), v) => schemaDefinitionError("Default value of variable was supplied both as attribute and element value: %s", node.toString)
  }

  override lazy val diagnosticChildren: DiagnosticsList = definingAnnotationDiagnosticChildren

  lazy val extName = expandedNCNameToQName

  val (typeURI, typeLocalName) = XMLUtils.QName(node, typeQName, this)
  val extType = XMLUtils.expandedQName(typeURI, typeLocalName)

  lazy val newVariableInstance = VariableFactory.create(this, extName, extType, defaultValue, external, doc)

}

class DFDLNewVariableInstance(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl) // with NewVariableInstance_AnnotationMixin 
  {
  lazy val ref = getAttributeRequired("ref")
  lazy val defaultValue = getAttributeOption("defaultValue")

  lazy val gram: Gram = NewVariableInstanceStart(decl, this)
  lazy val endGram: Gram = NewVariableInstanceEnd(decl, this)

  override lazy val diagnosticChildren: DiagnosticsList = List(gram, endGram)

  lazy val (uri, localName) = XMLUtils.QName(decl.xml, ref, decl.schemaDocument)
  lazy val expName = XMLUtils.expandedQName(uri, localName)
  lazy val defv = decl.schema.schemaSet.getDefineVariable(uri, localName).getOrElse(
    this.schemaDefinitionError("Variable not found: %s", ref))

  lazy val newVariableInstance = defv.newVariableInstance

}

class DFDLSetVariable(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl) // with SetVariable_AnnotationMixin 
  {
  lazy val ref = getAttributeRequired("ref")
  lazy val attrValue = getAttributeOption("value")
  lazy val <dfdl:setVariable>{ eltChildren @ _* }</dfdl:setVariable> = node
  lazy val eltValue = eltChildren.text.trim
  lazy val value = (attrValue, eltValue) match {
    case (None, v) if (v != "") => v
    case (Some(v), "") => v
    case (Some(v), ev) if (ev != "") => decl.SDE("Cannot have both a value attribute and an element value: %s", node)
    case (None, "") => decl.SDE("Must have either a value attribute or an element value: %s", node)
  }

  lazy val (uri, localName) = XMLUtils.QName(decl.xml, ref, decl.schemaDocument)
  lazy val defv = decl.schema.schemaSet.getDefineVariable(uri, localName).getOrElse(
    schemaDefinitionError("Unknown variable: %s", ref))

  lazy val gram = gram_.value
  private lazy val gram_ = LV('gram) {
    SetVariable(decl, this)
  }
}

