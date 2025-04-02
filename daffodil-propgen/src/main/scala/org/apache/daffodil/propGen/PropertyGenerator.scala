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

package org.apache.daffodil.propGen

import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.InputStream
import scala.xml._

/**
 * Reads the XSD which describes DFDL annotations and properties. Generates scala source code text for
 * enumeration objects accordingly to provide for type-safety, which will cut down on maintenance effort
 * if properties evolve, etc.
 *
 * This does depend on the particular style in which the XSD is written. That is, there is no attempt here
 * to handle XSD generally. If someone was to grossly refactor or change the patterns used in the XSD, this code
 * will most definitely break.
 */

class PropertyGenerator(arg: Node) {

  val dfdlSchema = arg

  val excludedTypes = List(
    "AlignmentType",
    "BinaryBooleanFalseRepType",
    "BinaryBooleanTrueRepType",
    "EmptyElementParsePolicy",
    "FillByteType",
    "Property",
    "PropertyNameType",
    "PropertyType",
    "SeparatorSuppressionPolicy",
    "TextNumberBase",
    "TextOutputMinLength",
    "TextStandardExponentRep",
    "TunableEmptyElementParsePolicy",
    "TunableParseUnparsePolicyTunable",
    "TunableSuppressSchemaDefinitionWarnings",
    "TunableUnqualifiedPathStepPolicy",
    "dafint:daffodilAG"
  )

  val excludedAttributes = List(
    "EmptyElementParsePolicy",
    "SeparatorSuppressionPolicy",
    "TextNumberBase",
    "TextOutputMinLength",
    "TextStandardExponentRep"
  )

  def excludeType(name: String) = {
    excludedTypes.exists { _.toUpperCase == name.toUpperCase() }
  }

  def excludeAttribute(name: String) = {
    excludedAttributes.exists { _.toUpperCase == name.toUpperCase() }
  }

  def generate() = {
    val res = genAll()
    res
  }

  def genAll(): Seq[String] = {
    val allTopLevel = dfdlSchema.child
    val thunks = allTopLevel.map(node => {
      node.label match {
        case "element" => genElement(node)
        case "attributeGroup" => genAttributeGroup(node)
        case "simpleType" => genSimpleType(node)
        case "complexType" => genComplexType(node)
        case _ => {
          ""
        }
      }
    })
    thunks.toSeq
  }

  /**
   * get string value of a simple unprefixed attribute.
   *
   * TODO: move to some utilities library
   */
  def attr(xsElem: Node, attr: String) = {
    val ns = (xsElem \ ("@" + attr))
    if (ns.length != 1) None
    else Some(ns(0).text)
  }

  def genElement(e: Node): String = {
    val Some(name) = attr(e, "name")
    val typeName = attr(e, "type")
    typeName match {
      case None => genElementImmediateType(e, name)
      case Some(tname) => genElementNamedType(e, tname, name)
    }
  }

  def genElementNamedType(e: Node, typeName: String, name: String): String = {
    val elementType = stripDFDLPrefix(typeName)
    if (excludeType(name)) return ""
    val subAgs = e \\ "attributeGroup"
    val subRefAgs = subAgs.filter(ag => attr(ag, "ref") != None) // the "ref" attributeGroups
    // val subNames = subRefAgs.map(ag => stripSuffix("AG", stripDFDLPrefix(attr(ag, "ref"))))
    val subNames =
      subRefAgs.map(ag => stripDFDLPrefix(attr(ag, "ref").get)) // leave AG suffix on.
    val res =
      generatePropertyGroup(name + "_Annotation", Nil, elementType :: subNames.toList, Nil)
    val block = comment(e) + res + sep
    block
  }

  def genElementImmediateType(e: Node, name: String): String = {
    // construct a type name from element name as if it were <element name="foo" type="dfdl:DFDLFooType"/>
    val firstLetter = name.substring(0, 1).toUpperCase()
    val restLetters = name.substring(1)
    val initialUpperCaseName = firstLetter + restLetters
    val typeNameNoPrefix = "DFDL" + initialUpperCaseName + "Type"
    val typeNameWithPrefix = "dfdl:" + typeNameNoPrefix
    val ctNodeSeq = (e \ "complexType")
    val ct = ctNodeSeq(0)
    val ctGeneratedCode = genComplexTypeWithName(ct, typeNameNoPrefix)
    val eGeneratedCode = genElementNamedType(e, typeNameWithPrefix, name)
    val res = ctGeneratedCode + "\n" + eGeneratedCode
    res
  }

  def genSimpleType(st: Node): String = {
    val enumName = attr(st, "name").get
    val name = enumName.stripSuffix("Enum")
    // val name = enumName // leave suffix on.
    if (excludeType(name)) return ""
    if (excludedTypes.contains(name)) return ""
    val enumNodes = (st \\ "enumeration")
    val symNodes = enumNodes \\ "@value"
    val syms = symNodes.map(_.text)
    val res =
      if (syms.length == 0) {
        // there were no enumerations for this type.
        // decide what to generate based on the base type.
        val baseTypeNameNodeSeq = (st \ "restriction" \ "@base")
        val baseTypeName =
          if (baseTypeNameNodeSeq.length >= 1) baseTypeNameNodeSeq(0).text else "not recognized"

        baseTypeName match {
          case "xsd:int" => generateIntProperty(name)
          case _ => generateStringProperty(name)
        }
      } else
        generateEnumProperty(name, syms)
    val block = comment(st) + res + sep
    block
  }

  def isScalaKeyword(s: String) = {
    val scalaKeywords = List(
      "type",
      "implicit",
      "class",
      "val",
      "extends",
      "with",
      "if",
      "else",
      "trait",
      "abstract"
    )
    val res = scalaKeywords.contains(s)
    res
  }

  def genAttributeGroup(ag: Node): String = {
    // val name = stripSuffix("AG", attr(ag, "name"))
    val name =
      attr(
        ag,
        "name"
      ).get // let's try leaving AG suffix in place so we can distinguish generated type mixins from AG mixins.
    if (excludeType(name)) return ""
    val subAgs = ag \ "attributeGroup"
    val subRefAgs = subAgs.filter(ag => attr(ag, "ref") != None)
    // val subNames = subRefAgs.map(ag => stripSuffix("AG", stripDFDLPrefix(attr(ag, "ref"))))
    val subNames =
      subRefAgs.map(ag => stripDFDLPrefix(attr(ag, "ref").get)) // leave AG suffix on.
    assert(subAgs.length == subRefAgs.length) // "nested attributeGroup was not a reference"
    val attribs = ag \ "attribute"
    //
    // for each attribute that is an Enum type, we want to use a Mixin of that type
    //
    val attribsNoDafRefs = attribs.flatMap { attrNode =>
      val rawRef = attr(attrNode, "ref")
      if (rawRef.isDefined) {
        // this is referencing a Daffodil Extension, go find that attribute

        if (rawRef.get.startsWith("daf:")) {
          // ignore daf prefix and the dfdlx:Layer attribute group. These are
          // duplicated in dfdlx for backwards compatability, so this ignores
          // the duplicate.
          None
        } else {
          val refWithoutPrefix = stripPrefix("dfdlx:", rawRef.get)
          val dafAttrNode = (PropertyGenerator.daffodilExtensionsXML \ "attribute").find {
            node =>
              attr(node, "name").get == refWithoutPrefix
          }.get
          Some(dafAttrNode)
        }
      } else {
        Some(attrNode)
      }
    }

    val nonExcludedAttribs = attribsNoDafRefs.filter { attrNode =>
      val rawName = attr(attrNode, "name").get
      !excludeAttribute(rawName)
    }

    val (enumAttributeList, nonEnumAttributeList) = nonExcludedAttribs.partition { attrNode =>
      {
        val qualifiedTypeName = attr(attrNode, "type").get
        val rawName = attr(attrNode, "name").get
        val rawNameWithoutTextPrefix = stripPrefix("text", rawName)
        val nameIsInTypeName =
          qualifiedTypeName.toLowerCase.contains(rawNameWithoutTextPrefix.toLowerCase)
        val endsInEnum = qualifiedTypeName.endsWith("Enum")
        val res = endsInEnum && nameIsInTypeName
        res
      }
    }

    val enumList = enumAttributeList.map { attrNode =>
      {
        val qualifiedTypeName = attr(attrNode, "type").get
        val enumName = stripSuffix("Enum", stripDFDLPrefix(qualifiedTypeName))
        enumName
      }
    }

    //
    // for other attributes, we generate a member
    //
    val attrNamesTypes = nonEnumAttributeList.flatMap(attrNode => {
      val rawName = attr(attrNode, "name").get
      //
      // exclude certain attribute names that aren't format properties
      // We don't want properties for these.
      val notFormatProperties =
        List("ref", "type", "name", "test", "defaultValue", "message", "baseFormat")
      val notScopedFormatProperties = List(
        "inputValueCalc",
        "outputValueCalc",
        "hiddenGroupRef"
      ) // do these by-hand since they are not scoped.
      val excludedBecauseDoneByHand =
        List(
          "runtimeProperties",
          "separatorPolicy",
          "separatorSuppressionPolicy",
          "textOutputMinLength",
          "textStandardExponentCharacter",
          "textStandardExponentRep",
          "textStandardInfinityRep",
          "textStandardNaNRep",
          "textStandardZeroRep",
          "nilValue",
          "textStringPadCharacter",
          "textNumberPadCharacter",
          "textBooleanPadCharacter",
          "textCalendarPadCharacter"
        )
      val exclusions =
        notFormatProperties ++ notScopedFormatProperties ++ excludedBecauseDoneByHand
      if (exclusions.contains(rawName)) {
        Nil
      } else {
        val name = if (isScalaKeyword(rawName)) rawName + "_" else rawName
        val qualifiedTypeName = attr(attrNode, "type").get
        // we still might have some enums here, because we exclude enums above that
        // aren't matching the name of the attribute. (e.g., if we have an attribute which has type YesNo, then
        // that will show up as a YesNoEnum here.
        val typeName = stripSuffix("Enum", stripDFDLPrefix(qualifiedTypeName))
        // val typeName = stripDFDLPrefix(qualifiedTypeName) // leave suffix on.
        List((name, typeName))
      }
    })

    val res = generatePropertyGroup(name, attrNamesTypes, subNames, enumList)
    val block = comment(ag) + res + sep
    block
  }

  def genComplexType(ct: Node): String = {
    val name = attr(ct, "name").get
    genComplexTypeWithName(ct, name)
  }

  def genComplexTypeWithName(ct: Node, name: String): String = {
    if (excludeType(name)) return ""
    val baseNames =
      (ct \\ "extension" \ "@base").map { base => stripDFDLPrefix(base.text) }.filterNot {
        _.startsWith("xsd:")
      }
    val subAgs = ct \\ "attributeGroup"
    val subRefAgs =
      subAgs.filter(ag => attr(ag, "ref") != None && !(excludeType(attr(ag, "ref").get)))
    // val subNames = subRefAgs.map(ag => stripSuffix("AG", stripDFDLPrefix(attr(ag, "ref"))))
    val subNames =
      subRefAgs.map(ag => stripDFDLPrefix(attr(ag, "ref").get)) // leave AG suffix on.

    val attribs = ct \\ "attribute"
    val attribNames =
      attribs.map(a => (attr(a, "name").get, stripDFDLPrefix(attr(a, "type").get)))
    val res = generatePropertyGroup(name, attribNames, baseNames ++ subNames, Nil)
    val block = comment(ct) + res + sep
    block
  }

  def stripDFDLPrefix(s: String) = {
    val s1 = stripPrefix("dfdl:", s)
    val s2 = stripPrefix("dfdlx:", s1)
    s2
  }

  def stripPrefix(prefix: String, source: String) = {
    if (source.startsWith(prefix)) {
      val r = source.split(prefix)
      r(1)
    } else source
  }

  def stripSuffix(suffix: String, source: String) = {
    if (source.endsWith(suffix)) {
      val r = source.split(suffix)
      r.head
    } else source
  }

  val sep = """////////////////////////////////////"""

  def comment(n: Node): String = {
    val res = n.toString.split("\n").toList.map("\n// " + _).reduce(_ + _)
    sep + res + "\n\n"
  }

  /**
   * Sweet idiom for generated enums courtesy the stack-overflow web site/discussions
   *
   * How to use. Suppose your property is dfdl:currency with values "eur" and "gbp":
   * <pre>
   * class User extends CurrencyMixin {
   * def myFunc() {
   * import Currency._ // if you want to name qualify then import.
   * val c = currency
   * val cs = c match {
   * case EUR => "EUR"
   * case GBP => "GBP"
   * }
   *
   * }
   * }
   * </pre>
   */

  val templateStart =
    """sealed trait Currency extends EnumValue
object Currency extends Enum[Currency] {
"""
  val templateMiddle =
    """  case object EUR extends Currency
"""

  // Modified to pass the context, so diagnostics can be better.
  val templateEnd = """
  def apply(name: String, context : ThrowsSDE) : Currency = stringToEnum("currency", name, context)
}"""
  val templateMixin = """

trait CurrencyMixin extends PropertyMixin {

  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for
   * property existence. Just insist on the property you need by
   * using its name. E.g., if you need calendarTimeZone, just use
   * a.calendarTimeZone (where a is an AnnotatedSchemaComponent)
   */
  private def optionCurrencyLookup = findPropertyOption("currency")
  final def optionCurrency = if (optionCurrencyLookup.isDefined) Some(currency) else None
  final def optionCurrency_location = if (optionCurrencyLookup.isDefined) Some(currency_location) else None

  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   *
   * Also gets the schema component where the property was found
   * so that one can report errors/diagnostics relative to that
   * location, not the point of use of the property.
   */
  private def currencyLookup = requireProperty(optionCurrencyLookup)
  final def currency = Currency(currencyLookup.value, this)
  final def currency_location = currencyLookup.location

  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */
  final def currencyToString() = {
    optionCurrency match {
      case None => "" // empty string if not present
      case Some(currency) =>  "currency='" + currency + "' "
    }
  }

  final def currencyInit() = {
    registerToStringFunction(() => currencyToString())
  }

  currencyInit() // call at object creation to initialize
}

"""

  /**
   * exclude these since code should use the CompiledExpression created from these values,
   * not the property values themselves. We'll do these by hand.
   */
  def excludeRuntimeProperties(propName: String) = {
    val runtimeValuedProperties = List(
      "byteOrder",
      "encoding",
      "initiator",
      "terminator",
      "outputNewLine",
      "length",
      "escapeCharacter",
      "escapeEscapeCharacter",
      "textStandardDecimalSeparator",
      "textStandardGroupingSeparator",
      "textStandardExponentRep",
      "binaryFloatRep",
      "textBooleanTrueRep",
      "textBooleanFalseRep",
      "separator",
      "occursCount",
      "inputValueCalc",
      "outputValueCalc",
      "calendarLanguage",
      "choiceDispatchKey"
    )
    val res = runtimeValuedProperties.contains(propName)
    res
  }

  def generateEnumProperty(pname: String, pvalues: Seq[String]) = {
    val traitName = initialUpperCase(pname)
    val propName = initialLowerCase(pname)
    val middle = templateMiddle.replaceAll("Currency", traitName)
    val pvalueIDs = pvalues.map(pvalue => initialUpperCase(pvalue))
    val mids = pvalueIDs.map(id => middle.replace("EUR", id))
    val values = pvalueIDs.mkString("  override lazy val values = Array(", ", ", ")\n")
    val start = templateStart.replaceAll("Currency", traitName)
    val end = templateEnd.replaceAll("Currency", traitName).replaceAll("currency", propName)
    val mixin =
      if (excludeRuntimeProperties(propName)) "\n"
      else {
        templateMixin.replaceAll("Currency", traitName).replaceAll("currency", propName)
      }
    val res = start + mids.foldLeft("")(_ + _) + values + end + mixin
    res
  }

  def generateEnumInstantiation(propName: String, typeName: String) = {
    val midTemplate =
      """
      final def EUR = Currency(findProperty("EUR").value, this)
      final def EUR_location = findProperty("EUR").location
"""
    val mid =
      if (excludeRuntimeProperties(propName)) ""
      else {
        midTemplate.replaceAll("Currency", typeName).replaceAll("EUR", propName)
      }
    mid
  }

  val stringPropertyTemplate = """
trait CurrencyMixin { /* nothing */ }
object Currency {
    def apply(s : String, self : ThrowsSDE) = s
}
"""

  val intPropertyTemplate = """
trait CurrencyMixin { /* nothing */ }
object Currency {
    def apply(s : String, self : ThrowsSDE) = s.toInt
}
"""

  def generateStringProperty(pname: String) = {
    val traitName = initialUpperCase(pname)
    val propName = initialLowerCase(pname)
    val res =
      stringPropertyTemplate.replaceAll("Currency", traitName).replaceAll("currency", propName)
    res
  }

  def generateIntProperty(pname: String) = {
    val traitName = initialUpperCase(pname)
    val propName = initialLowerCase(pname)
    val res =
      intPropertyTemplate.replaceAll("Currency", traitName).replaceAll("currency", propName)
    res
  }

  def isXSDTypeName(typeName: String) = {
    val arr = typeName.split(":")
    arr match {
      case Array(prefix, otherPart) => (prefix == "xsd")
      case _ => false
    }
  }

  def getConverterTypeName(typeName: String): String = {
    val Array(prefix, name) = typeName.split(":")
    assert(prefix == "xsd")
    val converterName = name match {
      case "boolean" => "Boolean"
      case "NCName" => "NCName"
      case "QName" => "QName"
      case "string" => "String"
      case "positiveInteger" => "Int"
      case "nonNegativeInteger" => "Int"
      case "int" => "Int"
      case "double" => "Double"
      case "float" => "Float"
      case _ => assert(false, "unrecognized type " + typeName); ""
    }
    converterName
  }

  def generateNonEnumInstantiation(propName: String, typeName: String) = {
    val converterName = getConverterTypeName(typeName)
    val midTemplate = if (converterName != "QName") {
      """  def EUR = convertToTYPE(findProperty("EUR").value)
"""
    } else {
      """
  def EUR = {
    val cp = findProperty("EUR")
    convertToTYPE(cp.value, cp.location)
  }
  def EUROption = findPropertyOption("EUR") match {
    case Found(s, ll, _, _) => Some(convertToTYPE(s, ll))
    case _ => None
  }
"""
    }

    val mid =
      if (excludeRuntimeProperties(propName)) ""
      else {
        midTemplate.replaceAll("TYPE", converterName).replaceAll("EUR", propName)
      }
    mid
  }

  def isEnumQName(str: String) = {
    str.startsWith("dfdl:") && str.endsWith("Enum")
  }

  def generatePropertyGroup(
    pgName: String,
    pgList: Seq[(String, String)],
    agList: Seq[String],
    enumList: Seq[String]
  ) = {
    val traitName = initialUpperCase(pgName)
    val traitNames = (enumList ++ agList).map(initialUpperCase(_) + "Mixin")
    val extendsClause = "extends PropertyMixin" + traitNames.foldLeft("")(_ + "\n  with " + _)
    val mixinName = traitName + "Mixin"
    val start = "trait " + mixinName + " " + extendsClause + " {\n"
    val (_ /* enumAttrList */, nonEnumAttrList) = pgList.partition {
      case (attrName, _) => {
        isEnumQName(attrName)
      }
    }
    val (primAttrList, nonPrimAttrList) = nonEnumAttrList.partition {
      case (attrName, attrTypeName) => isXSDTypeName(attrTypeName)
    }
    val primAttribsList = primAttrList.map {
      case (attrName, attrTypeName) => {
        val propName = initialLowerCase(attrName)
        val res =
          generateNonEnumInstantiation(propName, attrTypeName)
        res
      }
    }
    val nonPrimAttribsList = nonPrimAttrList.map {
      case (attrName, attrTypeName) => {
        val propName = initialLowerCase(attrName)
        val res =
          generateEnumInstantiation(propName, attrTypeName)
        res
      }
    }
    val initToStringFuncs = nonEnumAttrList.map { case (attrName, attrTypeName) =>
      val propName = initialLowerCase(attrName)
      generateNonEnumStringPropInit(propName)

    }
    val nonEnumInit = generateNonEnumStringInit(pgName, initToStringFuncs)

    val end = "}\n\n"
    val res = start + ( // enumAttribsList ++ // those are done via mixins now.
      primAttribsList ++ nonPrimAttribsList
    ).foldLeft("")(_ + _) + nonEnumInit + end
    res
  }

  /**
   * For properties that are not Enum types. We still need to initialize so that
   * we get a toString behavior that displays these properties.
   */
  def generateNonEnumStringPropInit(propName: String) = {
    val template =
      """registerToStringFunction(()=>{getPropertyOption("currency", expressionAllowed) match {
        case None => ""
        case Some(value) => "currency='" + value.toString + "'"
      }
    })"""
    val expressionAllowStr = excludeRuntimeProperties(propName).toString
    val res =
      template
        .replaceAll("currency", propName)
        .replaceAll("expressionAllowed", expressionAllowStr)
    res
  }

  /**
   * For a whole mixin we need an initializer which runs the initialization thunks for
   * all the properties the mixin contains.
   */
  def generateNonEnumStringInit(pgName: String, propInits: Seq[String]) = {
    val initFuncName = initialLowerCase(pgName)
    """
  def """ + initFuncName + """Init(): Unit = {""" +
      propInits.foldLeft("")(_ + """
    """ + _) + """
  }
  """ + initFuncName + """Init()
"""
  }

  //
  // These little utilities are repeated here rather than used from daffodil-lib
  // due to circular dependency problem. If you edit the code generator, and it creates bad code
  // that you then incorporate into daffodil-lib, and this generator is dependent on daffodil-lib
  // you'd be stuck.
  //
  // So this generator depends on as little as possible
  //

  def stripQuotes(s: String) = {
    val stripFirst = if (s.startsWith("\"")) s.substring(1) else s
    val stripLast =
      if (stripFirst.endsWith("\"")) stripFirst.substring(0, stripFirst.length - 1)
      else stripFirst
    stripLast
  }

  def initialUpperCase(s: String): String = s.head.toUpper +: s.substring(1)
  def initialLowerCase(s: String): String = {
    // special case for the way we lowercase the utf16Width property.
    if (s == "UTF16Width") "utf16Width"
    else
      s.head.toLower +: s.substring(1)
  }

} // end trait

object PropertyGenerator {
  val dfdlSchemasForDFDLAnnotations = List(
    "/org/apache/daffodil/xsd/DFDL_part1_simpletypes.xsd",
    "/org/apache/daffodil/xsd/DFDL_part2_attributes.xsd",
    "/org/apache/daffodil/xsd/DFDL_part3_model.xsd",
    "/org/apache/daffodil/xsd/dfdlx.xsd"
  )

  val daffodilConfigXML = getSchemaAsNode("/org/apache/daffodil/xsd/dafext.xsd")
  val daffodilExtensionsXML = getSchemaAsNode("/org/apache/daffodil/xsd/dfdlx.xsd")

  def getSchemaAsNode(name: String): Node = {
    val is = getResourceOrFileStream(name)
    val node = XML.load(is)
    node
  }

  def generatedCodeFilename = "GeneratedCode.scala"
  def generatedCodePackage = "org.apache.daffodil.lib.schema.annotation.props.gen"

  def tunableCodeFilename = "DaffodilTunablesGen.scala"
  def tunableCodePackage = "org.apache.daffodil.lib.iapi"

  def warnIdCodeFilename = "WarnIdGen.scala"
  def warnIdCodePackage = "org.apache.daffodil.lib.iapi"

  def preamble = "package " + generatedCodePackage + """

////////////////////////////////////////////////////////////////////////////////////////////
//
// Generated Code - Do not hand modify!
//
// This file is entirely generated code created from the
// XML Schema files that describe DFDL Annotation syntax.
//
// Don't edit this. Go fix the generator to create what you need instead.
//
////////////////////////////////////////////////////////////////////////////////////////////

import org.apache.daffodil.lib.schema.annotation.props._
import org.apache.daffodil.lib.exceptions.ThrowsSDE

"""

  def writeGeneratedCode(thunks: Seq[String], ow: java.io.FileWriter): Unit = {
    ow.write(preamble)
    for (thunk <- thunks) {
      ow.write(thunk)
      ow.flush()
    }
    ow.close()
  }

  def generateThunks() = {
    val allThunks = dfdlSchemasForDFDLAnnotations.flatMap { fname =>
      val schemaNode = getSchemaAsNode(fname)
      val oneSchemaFileThunks = new PropertyGenerator(schemaNode).generate()
      oneSchemaFileThunks
    }
    allThunks
  }

  def getGeneratedFilePath(rootDir: String, pkg: String, filename: String): String = {
    val outDir = new java.io.File(rootDir + "/" + pkg.split('.').reduceLeft(_ + "/" + _))
    outDir.mkdirs()
    val outPath = s"$outDir/$filename"
    outPath
  }

  /**
   * Main - run as a scala application to actually create a new GeneratedCode.scala file in the gen directory.
   */
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.exit(1);
    }

    val thunks = generateThunks()

    val generatedCodePath =
      getGeneratedFilePath(args(0), generatedCodePackage, generatedCodeFilename)
    writeGeneratedCode(thunks, new java.io.FileWriter(generatedCodePath))
    System.out.println(generatedCodePath)

    val tunablePath = getGeneratedFilePath(args(0), tunableCodePackage, tunableCodeFilename)
    val tunableGenerator = new TunableGenerator(daffodilConfigXML, daffodilExtensionsXML)
    tunableGenerator.writeGeneratedCode(new java.io.FileWriter(tunablePath))
    System.out.println(tunablePath)

    val warnIdPath = getGeneratedFilePath(args(0), warnIdCodePackage, warnIdCodeFilename)
    val warnIdGenerator = new WarnIDGenerator(daffodilConfigXML)
    warnIdGenerator.writeGeneratedCode(new java.io.FileWriter(warnIdPath))
    System.out.println(warnIdPath)

  }

  //
  // These little utilities are repeated here rather than used from daffodil-lib
  // due to circular dependency problem. If you edit the code generator, and it creates bad code
  // that you then incorporate into daffodil-lib, and this generator is dependent on daffodil-lib
  // you'd be stuck.
  //
  // So this generator depends on as little as possible
  //
  /**
   * Takes care of using the resource built-in to the jar, or
   * if we're just running interactively in eclipse, doesn't use the jar.
   */
  def getResourceOrFileStream(fn: String): InputStream = {
    // TODO: This is not the modern way to do this. Update this to use the getResource technique used
    // in core.
    var is = this.getClass().getResourceAsStream(fn)
    if (is == null) {
      is = new FileInputStream(fn)
      if (is == null) {
        val e = new FileNotFoundException(fn)
        throw e
      }
    }
    return is
  }
}
