package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.exceptions._
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
import daffodil.xml._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
import daffodil.processors._
import daffodil.dsom._
import daffodil.grammar._
import daffodil.api.Diagnostic

/**
 * Base class for any DFDL annotation
 */
abstract class DFDLAnnotation(node: Node, annotatedSC: AnnotatedSchemaComponent)
  extends DiagnosticsProviding
  with GetAttributesMixin
  with ThrowsSDE {
  lazy val xml = node

  override def addDiagnostic(diag: Diagnostic) = annotatedSC.addDiagnostic(diag)

  lazy val context = annotatedSC
  //  match {
  //      case sc : SchemaComponent => sc 
  //      case _ => Assert.invariantFailed("should be a SchemaComponent")
  //    }

  lazy val prettyName = xml.prefix + ":" + xml.label
  lazy val path = annotatedSC.path + "::" + prettyName

  def SDE(id: String, args: Any*) = {
    throw new SchemaDefinitionError(Some(context), Some(this), id, args: _*)
  }

  def SDW(id: String, args: Any*): Unit = {
    val sdw = new SchemaDefinitionWarning(Some(context), Some(this), id, args: _*)
    context.addDiagnostic(sdw)
  }

  def subset(testThatWillThrowIfFalse: Boolean, args: Any*) = {
    if (!testThatWillThrowIfFalse) SDE("Subset ", args: _*)
  }

  /**
   * If there is no default namespace (that is, no xmlns="..." at all), then getQName("foo") should return ("", "foo").
   * If there is a default namespace with URI "defNS", then getQName("foo") should return ("defNS", "foo")
   * If there is no namespace definition for prefix bar, then getQName("bar:foo") should produce a schemaDefinitionError,
   * because this is a referential integrity error.
   * If there is a namespace definition for prefix bar of "barNS", then getQName("bar:foo") should return ("barNS", "foo")
   */
  def getQName(name: String): (String, String) = {
    val parts = name.split(":").toList
    val (prefix, localName) = parts match {
      case List(local) => (null, local)
      case List(pre, local) => (pre, local)
      case _ => Assert.impossibleCase()
    }
    val nsURI = xml.getNamespace(prefix) // should work even when there is no namespace prefix.

    // TODO line numbers - see: http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element

    if (nsURI == null && prefix == null)
      ("", localName)
    else if (nsURI == null)
      schemaDefinitionError("In QName " + name + ", the prefix " + prefix + " was not defined.")
    else
      (nsURI, localName)
  }

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
  lazy val escapeSchemeObject = {

  }

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

/**
 * Base class for annotations that carry format properties
 */
abstract class DFDLFormatAnnotation(node: Node, annotatedSC: AnnotatedSchemaComponent)
  extends DFDLAnnotation(node, annotatedSC)
  with RawCommonRuntimeValuedPropertiesMixin
  with RawEscapeSchemeRuntimeValuedPropertiesMixin {

  lazy val diagnosticChildren = Nil

  private[dsom] def attributesInNamespace(ns: String, n: Node) = n.attributes.filter { _.getNamespace(n) == ns }
  private[dsom] def dfdlAttributes(n: Node) = attributesInNamespace(XMLUtils.DFDL_NAMESPACE, n)

  private[dsom] def getLocalFormatRef(): String = {
    // We have to check if the ref exists in long form (dfdl:ref)
    // or short form (ref).

    // longForm references are not prefixed by a dfdl namespace
    // they are actually located in the annotation located
    // on the node (DFDLAnnotation) itself.
    val optRefLongForm = node.attribute("ref")
    optRefLongForm match {
      case Some(longRef) => return longRef.toString()
      case None => ""
    }
    // Applicable shortForm ref has a dfdl namespace prefix and is located
    // on the actual Schema Component.
    val optRefShortForm = annotatedSC.xml.attribute(XMLUtils.DFDL_NAMESPACE, "ref")
    optRefShortForm match {
      case Some(shortRef) => return shortRef.toString()
      case None => ""
    }
    return ""
  }

  private[dsom] def getLocalEscapeSchemeRef(): String = {
    val ref = combinedLocalProperties.get("escapeSchemeRef")
    ref match {
      case None => ""
      case Some(s) => s
    }
  }

  lazy val detailName = xml.label

  //
  // Always look for a local property first
  //
  // package private since we want to unit test these and put the test code in a different object.
  // (Note: I hate repeating the darn package name all over the place here....)
  private[dsom] def getLocalPropertyOption(name: String): Option[String] = {
    // TODO: This does not appear to get called in the case of property_scoping_11
    if (hasConflictingPropertyError) {
      SDE("Short and long form properties overlap: %s", conflictingProperties)
    }
    lazy val localProp = combinedLocalProperties.get(name)
    localProp
  }

  //
  // reference chain lookup
  //
  private[dsom] def getRefPropertyOption(qName: String, pName: String): Option[String] = {
    var refStack: Set[String] = Set.empty[String]
    lazy val props = getDefineFormatPropertiesByRef(qName, refStack)
    lazy val propOpt = props.get(pName) // Use get on a map (rather than find)
    propOpt
  }

  //
  // NoDefault - local then reference chain rooted here
  //
  private[dsom] def getPropertyOptionNoDefault(name: String): Option[String] = {
    val local = getLocalPropertyOption(name)
    local match {
      case Some(_) => local
      case None => {
        val ref = getLocalFormatRef()
        val ref2 = getLocalEscapeSchemeRef()
        if (ref.length() > 0) {
          val refChainProp = getRefPropertyOption(ref, name)
          refChainProp
        } else if (ref2.length() > 0) {
          val refChainProp = getRefPropertyOption(ref2, name)
          refChainProp
        } else { None }
      }
    }
  }

  //
  // default - get the lexically enclosing default format annotation  // and do a no-default lookup on it (which looks locally there, and on the reference chain rooted  // at that default format annotation.
  //
  private[dsom] def getDefaultPropertyOption(name: String): Option[String] = {
    val lexicalDefaultFormatAnnotation = annotatedSC.schemaDocument.formatAnnotation
    val prop = lexicalDefaultFormatAnnotation.getPropertyOptionNoDefault(name)
    // no default for the default.
    prop
  }

  //
  // This is the primary public entry point.
  //
  def getPropertyOption(name: String): Option[String] = {
    //
    // These asserts are here to keep straight usage of properties, which have scoping 
    // applied to finding them, and other attributes.
    //
    Assert.usage(name != "ref", name + " is not a format property")
    Assert.usage(name != "name", name + " is not a format property")
    Assert.usage(name != "type", name + " is not a format property")

    // TODO: Is this correct? Don't default properties always apply?
    val nonDef = getPropertyOptionNoDefault(name) // local and local ref chain
    nonDef match {
      case Some(_) => { nonDef }
      case None => {
        val default = getDefaultPropertyOption(name) // default and default ref chain
        default
      }
    }
  }

  lazy val shortFormProperties = {
    // shortForm properties should be prefixed by dfdl
    // Remove the dfdl prefix from the attributes so that they
    // can be properly combined later.
    val res = dfdlAttributes(annotatedSC.xml).asAttrMap.map {
      case (key: String, value: String) => (removePrefix(key), value)
    }
    res.toSet
  }

  lazy val longFormProperties = {
    // longForm Properties are not prefixed by dfdl
    val dfdlAttrs = dfdlAttributes(node).asAttrMap
    schemaDefinition(dfdlAttrs.isEmpty, "long form properties are not prefixed by dfdl:")
    node.attributes.asAttrMap.toSet
  }

  lazy val conflictingProperties =
    shortFormProperties.intersect(longFormProperties).union(
      shortFormProperties.intersect(elementFormProperties)).union(
        longFormProperties.intersect(elementFormProperties))

  lazy val hasConflictingPropertyError = conflictingProperties.size != 0

  lazy val combinedLocalProperties = {
    // We need this error to occur immediately! Didn't seem to be checked otherwise.
    if (this.hasConflictingPropertyError) { SDE("Short and long form properties overlap: %s", conflictingProperties) }
    shortFormProperties.union(longFormProperties).union(elementFormProperties).toMap
  }

  lazy val elementFormProperties = (xml \ "property").map {
    case p @ <dfdl:property>{ value }</dfdl:property> => ((p \ "@name").text, value.text)
    case _ => Assert.impossibleCase()
  }.toSet

  // Added by Taylor Wise
  //
  def combinePropertiesWithOverriding(localProps: Map[String, String], refProps: Map[String, String]): Map[String, String] = {
    var result: Map[String, String] = localProps
    // Iterate over the ref's properties, if we find that
    // a local instance of that property exists, add the local instance
    // otherwise add the ref instance
    refProps foreach {
      case (refKey, refValue) => {
        val foundProp = localProps.find { p => p._1 == refKey }
        foundProp match {
          case Some(thisProp) => { /* Local version exists, don't add */ }
          case None => { result += (refKey -> refValue) }
        } // end-prop-match
      } // end-for-each-case
    } // end-for-each
    result
  }

  // Added by Taylor Wise
  //
  def getDefineFormatPropertiesByRef(qName: String, refStack: Set[String]): Map[String, String] = {
    Assert.usage(qName.length() > 0)
    var props = Map.empty[String, String]
    var localRefStack = refStack.toSet[String]
    val qnamePair = getQName(qName)
    val (nsURI, localName) = qnamePair

    // Verify that we don't have circular references
    if (refStack.contains(localName)) {
      schemaDefinitionError("Circular reference detected for "
        + localName + " while obtaining Define Format Properties!\nStack: "
        + refStack.toString())
    }

    localRefStack = localRefStack + localName

    // Retrieve the defineFormat that matches this qName
    val ss = annotatedSC.schema.schemaSet

    val foundDF = ss.getDefineFormat(nsURI, localName)
    foundDF match {
      case Some(aDF) => {
        // Found a defineFormat, grab its format properties
        val aFormat = aDF.formatAnnotation

        val ref: String = aFormat.getLocalFormatRef()

        // Local format properties, we already retrieved the appropriate format ref
        // no need for ref or name here
        val formatProps = aFormat.combinedLocalProperties.filterNot(x => x._1 == "ref" || x._1 == "name")

        // Was a reference found?
        if (ref.length() > 0) {

          // Add the local properties to the props list
          formatProps foreach { case (key, value) => props += (key -> value) }

          // Has a ref, go get the ref's properties
          val refFormatProps = getDefineFormatPropertiesByRef(ref, localRefStack)

          val result: Map[String, String] = combinePropertiesWithOverriding(formatProps, refFormatProps)

          props = props ++ result
        } else {
          // No ref, just return this format's properties
          props = formatProps
        } // end-if-else

      }
      case None => { this.SDE("Reference %s was not found. Format Reference Trace: %s", qName, localRefStack) }
    } // end-match
    props
  } // end-getDefineFormatProperties

  // Added by Taylor W.
  // 
  def getFormatProperties(): Map[String, String] = {

    // Fetch Local Format Properties
    val localProps = getFormatPropertiesNonDefault()

    // Fetch Default Format Properties
    val defaultProps = annotatedSC.schemaDocument.defaultFormat.combinedLocalProperties.filterNot(x => x._1 == "ref" || x._1 == "name")

    // Combine Local and Default properties via overriding
    val props = combinePropertiesWithOverriding(localProps, defaultProps)

    props
  } // end-getFormatProperties

  def removePrefix(prefixedValue: String): String = {
    if (prefixedValue.contains(":")) prefixedValue.substring(prefixedValue.indexOf(":") + 1).asInstanceOf[String]
    else prefixedValue
  }

  // Added by Taylor W.
  // 
  def getFormatPropertiesNonDefault(): Map[String, String] = {
    // Fetch Local properties but do not include ref and name
    val localProps = combinedLocalProperties.filterNot(x => x._1 == "ref" || x._1 == "name")

    // Remove the dfdl prefix from local properties so that they can be properly combined/overridden
    val localProps2 = localProps.map { case (key: String, value: String) => (removePrefix(key), value) }
    val localAndFormat = combinePropertiesWithOverriding(localProps2, formatRefProperties)
    //val props = combinePropertiesWithOverriding(localAndFormat, escapeSchemeRefProperties)
    localAndFormat
  } // end-getFormatPropertiesNonDefault

  lazy val formatRefProperties: Map[String, String] = {
    var refStack: Set[String] = Set.empty[String]
    var props: Map[String, String] = Map.empty[String, String]
    var ref = getLocalFormatRef()
    if (ref.length() != 0) {
      props = getDefineFormatPropertiesByRef(ref, refStack)
    }
    props
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
  with Format_AnnotationMixin
  with NillableMixin
  with SeparatorSuppressionPolicyMixin
  with RawElementRuntimeValuedPropertiesMixin
  with RawSequenceRuntimeValuedPropertiesMixin {

  override private[dsom] def getLocalFormatRef(): String = {
    val optRefNode = node.attribute("ref")
    optRefNode match {
      case Some(refNode) => return refNode.toString()
      case None => ""
    }
  }
}

class DFDLElement(node: Node, decl: ElementBase)
  extends DFDLFormatAnnotation(node, decl)
  with Element_AnnotationMixin
  with NillableMixin
  with RawElementRuntimeValuedPropertiesMixin {
}

class DFDLGroup(node: Node, decl: GroupBase)
  extends DFDLFormatAnnotation(node, decl)
  with Group_AnnotationMixin
  with SeparatorSuppressionPolicyMixin
  with RawSequenceRuntimeValuedPropertiesMixin {
}

class DFDLSequence(node: Node, decl: Sequence)
  extends DFDLFormatAnnotation(node, decl)
  with Sequence_AnnotationMixin
  with SeparatorSuppressionPolicyMixin
  with RawSequenceRuntimeValuedPropertiesMixin {
}

class DFDLChoice(node: Node, decl: Choice)
  extends DFDLFormatAnnotation(node, decl)
  with Choice_AnnotationMixin
  with RawSequenceRuntimeValuedPropertiesMixin {
  subset(getPropertyOptionNoDefault("initiator") == None, "initiators are not supported on choices")
  subset(getPropertyOptionNoDefault("terminator") == None, "terminators are not supported on choices")
}

class DFDLSimpleType(node: Node, decl: SimpleTypeDefBase)
  extends DFDLFormatAnnotation(node, decl)
  with SimpleType_AnnotationMixin
  with TextNumberFormatMixin
  with StringTextMixin
  with NumberTextMixin
  with CalendarTextMixin
  with BooleanTextMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin {
}

trait DFDLDefiningAnnotation { self: DFDLAnnotation =>
  lazy val nom = getAttributeRequired("name") // validation will check this for us.
  override lazy val prettyName: String = nom // note: name is not a format property.
  lazy val name: String = nom

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

  lazy val diagnosticChildren = formatAnnotation +: definingAnnotationDiagnosticChildren
}

class DFDLEscapeScheme(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLFormatAnnotation(node, decl)
  with EscapeScheme_AnnotationMixin
  with RawEscapeSchemeRuntimeValuedPropertiesMixin {
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

  lazy val diagnosticChildren = escapeScheme +: definingAnnotationDiagnosticChildren

  override def toString(): String = {
    "DFDLDefineEscapeScheme." + name
  }
}

abstract class DFDLAssertionBase(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl) {
  private lazy val testAttrib = getAttributeOption("test")
  private[dsom] lazy val testBody: Option[String] = node.child.text match {
    case s if (s.length() == 0) => {
      None
    }
    case txt => Some(txt)
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
  lazy val defaultValueAsElement = node.child.text
  lazy val defaultValue = (defaultValueAsAttribute, defaultValueAsElement) match {
    case (None, "") => None
    case (None, str) => Some(str)
    case (Some(str), "") => Some(str)
    case (Some(str), v) => schemaDefinitionError("Default value of variable was supplied both as attribute and element value: %s", node.toString)
  }

  override lazy val diagnosticChildren = definingAnnotationDiagnosticChildren

  lazy val qname = getQName(name)
  lazy val (uri, localName) = qname
  lazy val extName = XMLUtils.expandedQName(uri, localName)

  val (typeURI, typeLocalName) = XMLUtils.QName(node, typeQName, doc)
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
  lazy val eltValue = eltChildren.text
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

