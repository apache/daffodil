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

/**
 * Base class for any DFDL annotation
 */
abstract class DFDLAnnotation(node: Node, annotatedSC: AnnotatedMixin)
  extends DiagnosticsProviding
  with GetAttributesMixin {
  lazy val xml = node
  
  lazy val prettyName = xml.prefix + ":" + xml.label 
  lazy val path = annotatedSC.path + "::" + prettyName
}

trait RawCommonRuntimeValuedPropertiesMixin
  extends PropertyMixin {
  lazy val byteOrderRaw = getProperty("byteOrder")
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

  // TODO: Implement escape schemes. The escapeCharacter and escapeEscapeCharacter are part of the escapeScheme annotation only.

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
abstract class DFDLFormatAnnotation(node: Node, annotatedSC: SchemaComponent with AnnotatedMixin)
  extends DFDLAnnotation(node, annotatedSC)
  with RawCommonRuntimeValuedPropertiesMixin
  with RawEscapeSchemeRuntimeValuedPropertiesMixin {

  lazy val diagnosticChildren = Nil

  private[dsom] def getLocalFormatRef(): String = {
    val ref = combinedLocalProperties.get("ref")
    ref match {
      case None => ""
      case Some(s) => s
    }
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
    if (hasConflictingPropertyError) {
      Assert.SDE("Short and Long form properties overlap: " + conflictingProperties)
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
        } 
        else if (ref2.length() > 0 ) {
          val refChainProp = getRefPropertyOption(ref2, name)
          refChainProp
        }else { None }
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
    val nonDef = getPropertyOptionNoDefault(name) // local and local ref chain
    nonDef match {
      case Some(_) => nonDef
      case None => {
        val default = getDefaultPropertyOption(name) // default and default ref chain
        default
      }
    }
  }

  lazy val shortFormProperties = {
    val xsA = annotatedSC.xml
    xsA.attributes.asAttrMap.map {
      case (prop_name, value) if (prop_name.contains("dfdl:")) => (prop_name.replace("dfdl:", ""), value)
      case x => x
    }
  }.toSet

  lazy val longFormProperties = {
    val res = node.attributes.asAttrMap.toSet // No colon in name
    res
  }

  lazy val conflictingProperties =
    shortFormProperties.intersect(longFormProperties).union(
      shortFormProperties.intersect(elementFormProperties)).union(
        longFormProperties.intersect(elementFormProperties))

  lazy val hasConflictingPropertyError = conflictingProperties.size != 0

  lazy val combinedLocalProperties = shortFormProperties.union(longFormProperties).union(elementFormProperties).toMap

  lazy val elementFormProperties = (xml \ "property").map {
    case p @ <dfdl:property>{ value }</dfdl:property> => ((p \ "@name").text, value.text)
    case _ => Assert.impossibleCase()
  }.toSet

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
    // Assert.schemaDefinition(nsURI != null, "In QName " + name + ", the prefix " + prefix + " was not defined.")
    // TODO: accumulate errors, don't just throw on one.
    // TODO: error location for diagnostic purposes. 
    // see: http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
    if (nsURI == null && prefix == null)
      ("", localName)
    else if (nsURI == null)
      Assert.schemaDefinitionError("In QName " + name + ", the prefix " + prefix + " was not defined.")
    else
      (nsURI, localName)
  }

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
      Assert.schemaDefinitionError("Circular reference detected for "
        + localName + " while obtaining Format Properties!\nStack: "
        + refStack.toString())
    }

    localRefStack = localRefStack + localName

    // Retrieve the defineFormat that matches this qName
    val ss = annotatedSC.schema.schemaSet
    val dfs = ss.getDefineFormats(nsURI)
    
    //val refEscapeSchemeProps = this.escapeSchemeRefProperties // TODO: EscapeScheme

    val foundDF = dfs.find { df => df.name == localName }
    foundDF match {
      case Some(aDF) => {
        // Found a defineFormat, grab its format properties
        val aFormat = aDF.formatAnnotation

        val ref: String = aFormat.getLocalFormatRef()

        // Does this format have a ref?
        if (ref.length() > 0) {
          // Local format properties
          val formatProps = aFormat.combinedLocalProperties

          // Add the local properties to the props list
          formatProps foreach { case (key, value) => props += (key -> value) }

          // Has a ref, go get the ref's properties
          val refFormatProps = getDefineFormatPropertiesByRef(ref, localRefStack)
          
          val result: Map[String, String] = combinePropertiesWithOverriding(formatProps, refFormatProps)
          
          props = props ++ result
        } else {
          // No ref, just return this format's properties
          val formatProps = aFormat.combinedLocalProperties
          props = formatProps
        } // end-if-else

      }
      case None => { /* Do Nothing */ }
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


  // Added by Taylor W.
  // 
  def getFormatPropertiesNonDefault(): Map[String, String] = {

    // Fetch Local Format Properties
    val localProps = combinedLocalProperties.filterNot(x => x._1 == "ref" || x._1 == "name")
    val localAndFormat = combinePropertiesWithOverriding(localProps, formatRefProperties)
    //val props = combinePropertiesWithOverriding(localAndFormat, escapeSchemeRefProperties)
    localAndFormat
  } // end-getFormatPropertiesNonDefault

  lazy val formatRefProperties: Map[String, String] = {
    var refStack: Set[String] = Set.empty[String]
    var props: Map[String, String] = Map.empty[String, String]
    var ref = getLocalFormatRef()
    if (ref.length() == 0) {
      val found = this.combinedLocalProperties.filter(x => x._1 == "ref")
      if (found.size > 0) {
        ref = found.toSeq(0)._2
      }
    }
    if (ref.length() != 0) {
      props = getDefineFormatPropertiesByRef(ref, refStack)
    }
    props
  }

//  lazy val escapeSchemeRefProperties: Map[String, String] = {
//    var refStack: Set[String] = Set.empty[String]
//    var props: Map[String, String] = Map.empty[String, String]
//    var ref = getLocalEscapeSchemeRef()
//    if (ref.length() == 0) {
//      val found = this.combinedLocalProperties.filter(x => x._1 == "escapeSchemeRef")
//      if (found.size > 0) {
//        ref = found.toSeq(0)._2
//      }
//    }
//    if (ref.length() != 0) {
//      props = getDefineEscapeSchemePropertiesByRef(ref, refStack)
//    }
//    //props.filterNot(x => x._1 == "escapeSchemeRef")
//    props
//  }

//  // Added by Taylor Wise
//  //
//  def getDefineEscapeSchemePropertiesByRef(qName: String, refStack: Set[String]): Map[String, String] = {
//    Assert.usage(qName.length() > 0)
//    var props = Map.empty[String, String]
//    var localRefStack = refStack.toSet[String]
//    val qnamePair = getQName(qName)
//    val (nsURI, localName) = qnamePair
//
//    // Verify that we don't have circular references
//    if (refStack.contains(localName)) {
//      Assert.schemaDefinitionError("Circular reference detected for "
//        + localName + " while obtaining Escape Scheme Properties!\nStack: "
//        + refStack.toString())
//    }
//
//    localRefStack = localRefStack + localName
//
//    // Retrieve the defineFormat that matches this qName
//    val ss = annotatedSC.schema.schemaSet
//    val des = ss.getDefineEscapeScheme(nsURI, localName)
//
//    des match {
//      case Some(aES) => {
//        // Found a escapeScheme
//        val es = aES.escapeScheme
//
//        val ref: String = es.getLocalEscapeSchemeRef()
//
//        // Does this escapeScheme have a ref?
//        if (ref.length() > 0) {
//          // Local format properties
//          val esProps = es.combinedLocalProperties
//
//          // Add the local properties to the props list
//          esProps foreach { case (key, value) => props += (key -> value) }
//
//          // Has a ref, go get the ref's properties
//          val refFormatProps = getDefineEscapeSchemePropertiesByRef(ref, localRefStack)
//
//          val result: Map[String, String] = combinePropertiesWithOverriding(esProps, refFormatProps)
//          props = props ++ result
//        } else {
//          // No ref, just return this format's properties
//          val esProps = es.combinedLocalProperties
//          props = esProps
//        } // end-if-else
//
//      }
//      case None => { /* Do Nothing */ }
//    } // end-match
//
//    props
//  } // end-getDefineEscapeSchemePropertiesByRef
  
  // def escapeCharacterExpr = ExpressionCompiler.compile('String, es.getProperty("escapeCharacter"))
  // def escapeEscapeCharacterExpr = ExpressionCompiler.compile('String, es.getProperty("escapeEscapeCharacter"))
}

/**
 * Base class for assertions, variable assignments, etc
 */
abstract class DFDLStatement(node: Node, annotatedSC: AnnotatedMixin)
  extends DFDLAnnotation(node, annotatedSC) {
  
  lazy val diagnosticChildren : DiagnosticsList = Nil
}

class DFDLFormat(node: Node, sd: SchemaDocument)
  extends DFDLFormatAnnotation(node, sd)
  with Format_AnnotationMixin
  with SeparatorSuppressionPolicyMixin
  with RawElementRuntimeValuedPropertiesMixin
  with RawSequenceRuntimeValuedPropertiesMixin {
}

class DFDLElement(node: Node, decl: ElementBase)
  extends DFDLFormatAnnotation(node, decl)
  with Element_AnnotationMixin
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
  Assert.subset(getPropertyOptionNoDefault("initiator") == None, "initiators are not supported on choices")
  Assert.subset(getPropertyOptionNoDefault("terminator") == None, "terminators are not supported on choices")
}

class DFDLSimpleType(node: Node, decl: SimpleTypeBase)
  extends DFDLFormatAnnotation(node, decl)
  with SimpleType_AnnotationMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin {
}

trait DFDLDefiningAnnotation { self : DFDLAnnotation =>
  lazy val nom = getAttributeRequired("name") // validation will check this for us.
  override lazy val prettyName : String = nom // note: name is not a format property.
  lazy val name : String = nom
  
  lazy val definingAnnotationDiagnosticChildren : DiagnosticsList = Nil
}

class DFDLDefineFormat(node: Node, sd: SchemaDocument)
  extends DFDLAnnotation(node, sd) // Note: DefineFormat is not a format annotation
  with DFDLDefiningAnnotation
  // with DefineFormat_AnnotationMixin // mixins are only for format annotations.
  {

  lazy val baseFormat = getAttributeOption("baseFormat") // nor baseFormat

  lazy val formatAnnotation = LV{Utility.trim(node) match {
    case <dfdl:defineFormat>{ f @ <dfdl:format>{ contents @ _* }</dfdl:format> }</dfdl:defineFormat> =>
      new DFDLFormat(f, sd)
    case _ => Assert.impossibleCase()
  }
  }
  
  lazy val diagnosticChildren = formatAnnotation +: definingAnnotationDiagnosticChildren
}

class DFDLEscapeScheme(node: Node, decl: SchemaComponent with AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl)
  with EscapeScheme_AnnotationMixin
  with RawEscapeSchemeRuntimeValuedPropertiesMixin {
}

class DFDLDefineEscapeScheme(node: Node, decl: SchemaDocument)
  extends DFDLAnnotation(node, decl) // Note: defineEscapeScheme isn't a format annotation itself.
  with DFDLDefiningAnnotation
  // with DefineEscapeScheme_AnnotationMixin 
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

abstract class DFDLAssertionBase(node: Node, decl: AnnotatedMixin)
  extends DFDLStatement(node, decl) {

  lazy val testBody = node.child.text
  lazy val testPattern = getAttributeOption("testPattern")
  lazy val message = getAttributeOption("message")
  lazy val test = getAttributeOption("test")
  //
  // TODO: override diagnosticChildren if we compile the testBody/pattern into an object
  // which can provide error/diagnostic information itself (beyond what this class itself
  // can provide... which is nothing right now, but it could...someday).
}

class DFDLAssert(node: Node, decl: AnnotatedMixin)
  extends DFDLAssertionBase(node, decl) // with Assert_AnnotationMixin // Note: don't use these generated mixins. Statements don't have format properties                                  
  {
  // all attributes come from base class
}

class DFDLDiscriminator(node: Node, decl: AnnotatedMixin)
  extends DFDLAssertionBase(node, decl) // with Discriminator_AnnotationMixin 
  {
  // all attributes come from base class
}

class DFDLDefineVariable(node: Node, decl: AnnotatedMixin)
  extends DFDLStatement(node, decl) //with DefineVariable_AnnotationMixin 
  with DFDLDefiningAnnotation
  {
  //TODO: check: are the rest of these required or optional?
  lazy val predefined = getAttributeOption("predefined")
  lazy val type_ = getAttributeOption("type")
  lazy val external = getAttributeOption("external")
  lazy val defaultValue = getAttributeOption("defaultValue")
  
  override lazy val diagnosticChildren = definingAnnotationDiagnosticChildren
}

class DFDLNewVariableInstance(node: Node, decl: AnnotatedMixin)
  extends DFDLStatement(node, decl) // with NewVariableInstance_AnnotationMixin 
  {
  lazy val ref = getAttributeRequired("ref")
  lazy val defaultValue = getAttributeOption("defaultValue")
}

class DFDLSetVariable(node: Node, decl: AnnotatedMixin)
  extends DFDLStatement(node, decl) // with SetVariable_AnnotationMixin 
  {
  lazy val ref = getAttributeRequired("ref")
  lazy val value = getAttributeRequired("value")
}


