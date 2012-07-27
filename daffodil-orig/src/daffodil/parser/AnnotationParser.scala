package daffodil.parser

import daffodil.schema.annotation.Format
/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/*
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 */

import regex.Regex
import scala.collection.mutable.Map

import org.jdom.Element
import org.jdom.Attribute


import daffodil.xml.XMLUtil
import daffodil.xml.XMLUtil._
import daffodil.exceptions.DFDLSchemaDefinitionException
import daffodil.exceptions.UnimplementedException
import daffodil.schema.annotation.Annotation
import daffodil.schema.annotation.Format
import daffodil.schema.annotation.Discrimination
import daffodil.schema.annotation.InputValue
import daffodil.schema.annotation.VariableBinding
import daffodil.schema.annotation.VariableDefinition
import daffodil.schema.annotation.Assertion
import daffodil.processors.xpath.XPathUtil
import java.util.regex.Pattern

/**
 *  Parses the  DFDL annotations in a single node in a schema
 *
 * @author Alejandro Rodriguez
 * @version 1
 */
object AnnotationParser extends Function2[Element,Map[String,Format],Annotation]{

  //DFDL attributes
  val DFDL_SOURCE = "http://www.ogf.org/dfdl/"

  val ALIGNMENT = "alignment"
  val ALIGNMENT_PREFIXED = DFDL_NAMESPACE+ALIGNMENT
  val ALIGNMENT_UNITS = "alignmentUnits"
  val ALIGNMENT_UNITS_PREFIXED = DFDL_NAMESPACE+ALIGNMENT_UNITS
  val BINARY_BOOLEAN_TRUE_REP = "binaryBooleanTrueRep"
  val BINARY_BOOLEAN_TRUE_REP_PREFIXED = DFDL_NAMESPACE+BINARY_BOOLEAN_TRUE_REP
  val BINARY_BOOLEAN_FALSE_REP = "binaryBooleanFalseRep"
  val BINARY_BOOLEAN_FALSE_REP_PREFIXED = DFDL_NAMESPACE+BINARY_BOOLEAN_FALSE_REP
  val BINARY_FLOAT_REPRESENTATION = "binaryFloatRepresentation"
  val BINARY_FLOAT_REPRESENTATION_PREFIXED = DFDL_NAMESPACE+BINARY_FLOAT_REPRESENTATION
  val BINARY_NUMBER_REPRESENTATION = "binaryNumberRepresentation"
  val BINARY_NUMBER_REPRESENTATION_PREFIXED = DFDL_NAMESPACE+BINARY_NUMBER_REPRESENTATION
  val BYTE_ORDER = "byteOrder"
  val BYTE_ORDER_PREFIXED = DFDL_NAMESPACE+BYTE_ORDER
  val CALENDAR_PATTERN = "calendarPattern"
  val CALENDAR_PATTERN_PREFIXED = DFDL_NAMESPACE+CALENDAR_PATTERN
  val CALENDAR_PATTERN_KIND = "calendarPatternKind"
  val CALENDAR_PATTERN_KIND_PREFIXED = DFDL_NAMESPACE+CALENDAR_PATTERN_KIND
  val CALENDAR_TIME_ZONE = "calendarTimeZone"
  val CALENDAR_TIME_ZONE_PREFIXED = DFDL_NAMESPACE+CALENDAR_TIME_ZONE
  val CHOICE_LENGTH = "choiceLength"
  val CHOICE_LENGTH_PREFIXED = DFDL_NAMESPACE+CHOICE_LENGTH
  val CHOICE_LENGTH_KIND = "choiceLengthKind"
  val CHOICE_LENGTH_KIND_PREFIXED = DFDL_NAMESPACE+CHOICE_LENGTH_KIND
  val DEFAULT = "default"
  val DEFAULT_PREFIXED = DFDL_NAMESPACE+DEFAULT // there is no annotation "dfdl:default", but there is an "xsd:default"
  val ENCODING = "encoding"
  val ENCODING_PREFIXED = DFDL_NAMESPACE+ENCODING
  val ESCAPE_BLOCK_START = "escapeBlockStart"
  val ESCAPE_BLOCK_START_PREFIXED = DFDL_NAMESPACE+ESCAPE_BLOCK_START
  val ESCAPE_BLOCK_END = "escapeBlockEnd"
  val ESCAPE_BLOCK_END_PREFIXED = DFDL_NAMESPACE+ESCAPE_BLOCK_END
  val ESCAPE_CHARACTER = "escapeCharacter"
  val ESCAPE_CHARACTER_PREFIXED = DFDL_NAMESPACE+ESCAPE_CHARACTER
  val ESCAPE_ESCAPE_CHARACTER = "escapeEscapeCharacter"
  val ESCAPE_ESCAPE_CHARACTER_PREFIXED = DFDL_NAMESPACE+ESCAPE_ESCAPE_CHARACTER
  val ESCAPE_KIND = "escapeKind"
  val ESCAPE_KIND_PREFIXED = DFDL_NAMESPACE+ESCAPE_KIND
  val ESCAPE_SCHEMA_REF = "escapeSchemeRef"
  val ESCAPE_SCHEMA_REF_PREFIXED = DFDL_NAMESPACE+ESCAPE_SCHEMA_REF
  val FINAL_TERMINATOR_CAN_BE_MISSING = "documentFinalTerminatorCanBeMissing"
  val FINAL_TERMINATOR_CAN_BE_MISSING_PREFIXED = DFDL_NAMESPACE+FINAL_TERMINATOR_CAN_BE_MISSING
  val IGNORE_CASE = "ignoreCase"
  val IGNORE_CASE_PREFIXED = DFDL_NAMESPACE+IGNORE_CASE
  val INITIATED_CONTENT = "initiatedContent"
  val INITIATED_CONTENT_PREFIXED = DFDL_NAMESPACE+INITIATED_CONTENT
  val INITIATOR = "initiator"
  val INITIATOR_PREFIXED = DFDL_NAMESPACE+INITIATOR
  val INPUT_VALUE_CALC = "inputValueCalc"
  val INPUT_VALUE_CALC_PREFIXED = DFDL_NAMESPACE+"inputValueCalc"
  val LEADING_SKIP_BYTES = "leadingSkipBytes"
  val LEADING_SKIP_BYTES_PREFIXED = DFDL_NAMESPACE + LEADING_SKIP_BYTES
  val LENGTH = "length"
  val LENGTH_PREFIXED = DFDL_NAMESPACE+LENGTH
  val LENGTH_KIND = "lengthKind"
  val LENGTH_KIND_PREFIXED = DFDL_NAMESPACE+LENGTH_KIND
  val LENGTH_PATTERN = "lengthPattern"
  val LENGTH_PATTERN_PREFIXED = DFDL_NAMESPACE+LENGTH_PATTERN
  val MAX_LENGTH = "maxLength"
  val MAX_LENGTH_PREFIXED = DFDL_NAMESPACE+MAX_LENGTH
  val MAX_OCCURS = "maxOccurs"
  val MAX_OCCURS_PREFIXED = DFDL_NAMESPACE+"maxOccurs"
  val MIN_OCCURS = "minOccurs"
  val MIN_OCCURS_PREFIXED = DFDL_NAMESPACE+"minOccurs"
  val NILLABLE = "nillable"
  val NUMBER_DECIMAL_SEPARATOR = "numberDecimalSeparator"
  val NUMBER_DECIMAL_SEPARATOR_PREFIXED = DFDL_NAMESPACE+"numberDecimalSeparator"
  val OCCURS_COUNT_KIND = "occursCountKind"
  val OCCURS_COUNT_KIND_PREFIXED = DFDL_NAMESPACE+"occursCountKind"
  val OCCURS_COUNT = "occursCount"
  val OCCURS_COUNT_PREFIXED = DFDL_NAMESPACE+"occursCount"
  val OCCURS_STOP_VALUE = "occursStopValue"
  val OCCURS_STOP_VALUE_PREFIXED = DFDL_NAMESPACE+"occursStopValue"
  val OUTPUT_VALUE_CALC = "outputValueCalc"
  val OUTPUT_VALUE_CALC_PREFIXED = DFDL_NAMESPACE+"outputValueCalc"
  val PREFIX_LENGTH_TYPE = "prefixLengthType"
  val PREFIX_LENGTH_TYPE_PREFIXED = DFDL_NAMESPACE+"prefixLengthType"
  val PREFIX_INCLUDES_PREFIX_LENGTH = "prefixIncludesPrefixLength"
  val PREFIX_INCLUDES_PREFIX_LENGTH_PREFIXED = DFDL_NAMESPACE+"prefixIncludesPrefixLength"
  val REF = "ref"
  val REF_PREFIXED = DFDL_NAMESPACE+REF
  val REPRESENTATION = "representation"
  val REPRESENTATION_PREFIXED = DFDL_NAMESPACE+REPRESENTATION
  val SEPARATOR = "separator"
  val SEPARATOR_PREFIXED = DFDL_NAMESPACE+SEPARATOR
  val SEPARATOR_POSITION = "separatorPosition"
  val SEPARATOR_POSITION_PREFIXED = DFDL_NAMESPACE+SEPARATOR_POSITION
  val SEPARATOR_POLICY = "separatorPolicy"
  val SEPARATOR_POLICY_PREFIXED = DFDL_NAMESPACE+SEPARATOR_POLICY
  val SEQUENCE_KIND = "sequenceKind"
  val SEQUENCE_KIND_PREFIXED = DFDL_NAMESPACE+SEQUENCE_KIND
  val STOP_VALUE = "stopValue"
  val STOP_VALUE_PREFIXED = DFDL_NAMESPACE+STOP_VALUE
  val TERMINATOR = "terminator"
  val TERMINATOR_PREFIXED = DFDL_NAMESPACE+TERMINATOR
  val TEXT_BOOLEAN_TRUE_REP = "textBooleanTrueRep"
  val TEXT_BOOLEAN_TRUE_REP_PREFIXED = DFDL_NAMESPACE+TEXT_BOOLEAN_TRUE_REP
  val TEXT_BOOLEAN_FALSE_REP = "textBooleanFalseRep"
  val TEXT_BOOLEAN_FALSE_REP_PREFIXED = DFDL_NAMESPACE+TEXT_BOOLEAN_FALSE_REP
  val TEXT_BOOLEAN_JUSTIFICATION = "textBooleanJustification"
  val TEXT_BOOLEAN_JUSTIFICATION_PREFIXED = DFDL_NAMESPACE+TEXT_BOOLEAN_JUSTIFICATION
  val TEXT_BOOLEAN_PAD_CHARACTER = "textBooleanPadCharacter"
  val TEXT_BOOLEAN_PAD_CHARACTER_PREFIXED = DFDL_NAMESPACE+TEXT_BOOLEAN_PAD_CHARACTER
  val TEXT_CALENDAR_FORMAT_REF =  "textCalendarFormatRef"
  val TEXT_CALENDAR_FORMAT_REF_PREFIXED = DFDL_NAMESPACE+TEXT_CALENDAR_FORMAT_REF
  val TEXT_CALENDAR_JUSTIFICATION = "textCalendarJustification"
  val TEXT_CALENDAR_JUSTIFICATION_PREFIXED = DFDL_NAMESPACE + TEXT_CALENDAR_JUSTIFICATION
  val TEXT_CALENDAR_PAD_CHARACTER = "textCalendarPadCharacter"
  val TEXT_CALENDAR_PAD_CHARACTER_PREFIXED = DFDL_NAMESPACE + TEXT_CALENDAR_PAD_CHARACTER
  val TEXT_NUMBER_BASE = "textNumberBase"
  val TEXT_NUMBER_BASE_PREFIXED = DFDL_NAMESPACE + TEXT_NUMBER_BASE
  val TEXT_NUMBER_CHECK_POLICY = "textNumberCheckPolicy"
  val TEXT_NUMBER_CHECK_POLICY_PREFIXED = DFDL_NAMESPACE+TEXT_NUMBER_CHECK_POLICY
  val TEXT_NUMBER_FORMAT_REF = "textNumberFormatRef"
  val TEXT_NUMBER_FORMAT_REF_PREFIXED = DFDL_NAMESPACE+TEXT_NUMBER_FORMAT_REF
  val TEXT_NUMBER_JUSTIFICATION = "textNumberJustification"
  val TEXT_NUMBER_JUSTIFICATION_PREFIXED = DFDL_NAMESPACE + TEXT_NUMBER_JUSTIFICATION
  val TEXT_NUMBER_PAD_CHARACTER = "textNumberPadCharacter"
  val TEXT_NUMBER_PAD_CHARACTER_PREFIXED = DFDL_NAMESPACE + TEXT_NUMBER_PAD_CHARACTER
  val TEXT_NUMBER_REP = "textNumberRep"
  val TEXT_NUMBER_REP_PREFIXED = DFDL_NAMESPACE + TEXT_NUMBER_REP
  val TEXT_NUMBER_PATTERN = "textNumberPattern"
  val TEXT_NUMBER_PATTERN_PREFIXED = DFDL_NAMESPACE + TEXT_NUMBER_PATTERN
  val TEXT_NUMBER_ROUNDING_MODE = "textNumberRoundingMode"
  val TEXT_NUMBER_ROUNDING_MODE_PREFIXED = DFDL_NAMESPACE + TEXT_NUMBER_ROUNDING_MODE
  val TEXT_STANDARD_BASE = "textStandardBase" // TODO - remove this obsolete property name
  val TEXT_STANDARD_BASE_PREFIXED = DFDL_NAMESPACE + TEXT_STANDARD_BASE
  val TEXT_STANDARD_DECIMAL_SEPARATOR = "textStandardDecimalSeparator"
  val TEXT_STANDARD_DECIMAL_SEPARATOR_PREFIXED = DFDL_NAMESPACE + TEXT_STANDARD_DECIMAL_SEPARATOR
  val TEXT_STANDARD_EXPONENT_CHARACTER = "textStandardExponentCharacter"
  val TEXT_STANDARD_EXPONENT_CHARACTER_PREFIXED = DFDL_NAMESPACE + TEXT_STANDARD_EXPONENT_CHARACTER
  val TEXT_STANDARD_INFINITY_REP = "textStandardInfinityRep"
  val TEXT_STANDARD_INFINITY_REP_PREFIXED = DFDL_NAMESPACE + TEXT_STANDARD_INFINITY_REP
  val TEXT_STANDARD_GROUPING_SEPARATOR = "textStandardGroupingSeparator"
  val TEXT_STANDARD_GROUPING_SEPARATOR_PREFIXED = DFDL_NAMESPACE + TEXT_STANDARD_GROUPING_SEPARATOR
  val TEXT_STANDARD_NAN_REP = "textStandardNanRep"
  val TEXT_STANDARD_NAN_REP_PREFIXED = DFDL_NAMESPACE + TEXT_STANDARD_NAN_REP
  val TEXT_STANDARD_ZERO_REP = "textStandardZeroRep"
  val TEXT_STANDARD_ZERO_REP_PREFIXED = DFDL_NAMESPACE + TEXT_STANDARD_ZERO_REP
  val TEXT_ZONED_SIGN_STYLE = "textZonedSignStyle"
  val TEXT_ZONED_SIGN_STYLE_PREFIXED = DFDL_NAMESPACE + TEXT_ZONED_SIGN_STYLE
  val TEXT_STRING_JUSTIFICATION = "textStringJustification"
  val TEXT_STRING_JUSTIFICATION_PREFIXED = DFDL_NAMESPACE + TEXT_STRING_JUSTIFICATION
  val TEXT_STRING_PAD_CHARACTER = "textStringPadCharacter"
  val TEXT_STRING_PAD_CHARACTER_PREFIXED = DFDL_NAMESPACE + TEXT_STRING_PAD_CHARACTER
  val TRAILING_SKIP_BYTES = "trailingSkipBytes"
  val TRAILING_SKIP_BYTES_PREFIXED = DFDL_NAMESPACE + TRAILING_SKIP_BYTES

  /**
   * Returns a dfdl:hidden element in an annotation if there is one present
   *
   * @param parent the node in the schema where to look for hidden elements
   * @param root the root of the schema
   * @returns an element if parent contained a dfdl:hidden element, null otherwise
   */
  def getHiddenElement(parent:Element,root:Element):Element = {
    for(c1 <- getChildren(parent)){
      getFullName(c1) match {
        case APP_INFO =>
          if (getAttribute(c1,"source") == Some(DFDL_SOURCE))
            for (c2 <- getChildren(c1))
              getFullName(c2) match {
                case DFDL_HIDDEN => getAttribute(c2,"groupref") match {
                  case Some(s) => return getChildByName(root,s)
                  case None =>  for (c3 <- getChildren(c2)) if (getFullName(c3) != PCDATA) return c3
                }
                case _ =>
              }
        case _ =>
      }
    }
    null
  }

  /**
   * Parses the annotation of a node in a DFDL schema.
   * DFDl:Hidden elements are not parsed by this function
   *
   * @param parent the node to be search for annotations
   * @param definitions all globally defined formats
   * @returns an Annotation object containing all the annotations in the node
   */
  override def apply(parent: Element, definitions: Map[String, Format]): Annotation = {
    var annotation = new Annotation(parent)
    parseAttributes(getAttributes(parent), parent, annotation, definitions, false)

    val children = getChildren(parent)
    for (immediateChild <- children) {
      val elemName = getFullNameWithNamespace(immediateChild)
      elemName match {
        case PCDATA | REM => { /* do nothing */ }
        case ANNOTATION => {
          val innerChildren = getChildren(immediateChild)
          for (c1 <- innerChildren) {
            val elemName = getFullNameWithNamespace(c1)
            elemName match {
              case PCDATA | REM =>
              case APP_INFO => {
                val actualSource = getAttribute(c1, "source")
                val acceptableSource: Boolean = actualSource match {
                  case Some(s) => {
                    s == DFDL_SOURCE ||
                      DFDL_SOURCE.startsWith(s) || // Let's tolerate inaccurate source attributes on appinfo for now.
                      s.startsWith(DFDL_SOURCE)    // tolerate either one being a prefix of the other
                  }
                  case _ => false // appinfo, but source attribute on it is not DFDL's source
                }
                if (acceptableSource) {
                  val dfdlChildren = getChildren(c1)
                  for (c2 <- dfdlChildren) {
                    val elemName = getFullNameWithNamespace(c2)
                    elemName match {
                      case PCDATA | REM => { /* do nothing */ }
                      case DFDL_FORMAT | DFDL_CHOICE | DFDL_SEQUENCE | DFDL_GROUP |
                        DFDL_ELEMENT | DFDL_ESCAPE_SCHEMA => parseFormat(c2, annotation, definitions)
                      case DFDL_SIMPLE_TYPE => throw new UnimplementedException("simpleType", schemaContext = c2) // TODO implment
                      case DFDL_DEFINE_ESCAPE_SCHEME => parseGlobalEscapeDefinition(c2, definitions)
                      case DFDL_DEFINE_FORMAT => parseGlobalFormatDefinition(c2, definitions)
                      //case DFDL_DEFINE_TEXT_NUMBER_FORMAT => parseGlobalFormatDefinition(c2,definitions)
                      //case DFDL_DEFINE_CALENDAR_FORMAT => parseGlobalCalendarDefinition(c2,definitions)
                      case DFDL_ASSERT => parseAssertion(annotation, c2)
                      case DFDL_DISCRIMINATOR => parseDiscriminator(annotation, c2)
                      case DFDL_DEFINE_VARIABLE | DFDL_NEW_VARIABLE_INSTANCE => parseVariableDefinition(annotation, c2)
                      case DFDL_SET_VARIABLE => parseVariableBinding(annotation, c2)
                      case DFDL_HIDDEN => { /* not processed here */ }
                      case _ => throw new DFDLSchemaDefinitionException("Unknown annotation " + getFullName(c2), schemaContext = c2) // TODO: don't throw, accumulate
                    }
                  }
                }
              }
            }
          }
        }
        case _ =>
      }
    }
    annotation
  }

  private def parseDefaultFormatDefinition(c2:Element,definitions:Map[String,Format]) = {
    val c3 = XMLUtil.getChildByTag(c2,DFDL_FORMAT)
    c3 match {
      case Some(e) =>
        val annotation = new Annotation(e)
        parseFormat(e,annotation,definitions)
        definitions += ((Format.DEFAULT_FORMAT,annotation.format))
      case None => throw new DFDLSchemaDefinitionException("Missing format element",schemaContext = c2)
    }
  }
  
  private def parseGlobalFormatDefinition(c2:Element,definitions:Map[String,Format]) = {
    val c3 = XMLUtil.getChildByTag(c2,DFDL_FORMAT)
    c3 match {
      case Some(e) =>
        val annotation = new Annotation(e)
        parseFormat(e,annotation,definitions)
        getAttribute(c2,"name") match {
          case Some(s) => definitions += ((s,annotation format))
          case None => throw new DFDLSchemaDefinitionException("Missing name for format definition",schemaContext = c2)
        }
      case None => throw new DFDLSchemaDefinitionException("Missing format element",schemaContext = c2)
    }
  }

  private def parseGlobalEscapeDefinition(c2:Element,definitions:Map[String,Format]) = {

    val c3 = XMLUtil.getChildByTag(c2,DFDL_NAMESPACE+"escapeScheme")
    c3 match {
      case Some(e) =>
        val annotation = new Annotation(e)
        parseFormat(e,annotation,definitions)
        getAttribute(c2,"name") match {
          case Some(s) => definitions += ((s,annotation format))
          case None => throw new DFDLSchemaDefinitionException("Missing name for EscapeScheme definition",schemaContext = c2)
        }
      case None => throw new DFDLSchemaDefinitionException("Missing escapeScheme element",schemaContext = c2)
    }
  }

  private def parseGlobalCalendarDefinition(c2:Element,definitions:Map[String,Format]) = {
    val c3 = XMLUtil.getChildByTag(c2,DFDL_NAMESPACE+"escapeScheme")
    c3 match {
      case Some(e) =>
        val annotation = new Annotation(e)
        parseFormat(e,annotation,definitions)
        getAttribute(c2,"name") match {
          case Some(s) => definitions += ((s,annotation format))
          case None => throw new DFDLSchemaDefinitionException("Missing name for EscapeScheme definition",schemaContext = c2)
        }
      case None => throw new DFDLSchemaDefinitionException("Missing escapeScheme element",schemaContext = c2)
    }

  }

  private def parseCalendar(c2:Element,annotation:Annotation,definitions:Map[String,Format]):Unit = {
    parseAttributes(getAttributes(c2),c2,annotation,definitions,true)

    //c getText -> unescape(c getText)

    for(c <- getChildren(c2))
      getFullName(c) match {
        case PCDATA | REM =>
        case DFDL_PROPERTY => getAttribute(c,"name") match {
          case Some(CALENDAR_PATTERN) => annotation.format.setCalendarPattern(c getText)
          case Some(CALENDAR_PATTERN_KIND) => annotation.format.setCalendarPatternKind(c getText)
          case Some(CALENDAR_TIME_ZONE) => annotation.format.setCalendarTimeZone(c getText)
          case _ => throw new DFDLSchemaDefinitionException("Unknown property "+c.getText,schemaContext = c)
        }
        case _ =>
      }
  }

  private def parseFormat(c2: Element, annotation: Annotation, definitions: Map[String, Format]): Unit = {
    parseAttributes(getAttributes(c2), c2, annotation, definitions, true)

    //c getText -> unescape(c getText)

    for (c <- getChildren(c2))
      try {
        getFullNameWithNamespace(c) match {
          case PCDATA | REM =>
          case DFDL_PROPERTY => {
            val propNameOption = getAttribute(c, "name")
            val propName = propNameOption match {
              case Some(name) => name
              case _ => throw new Exception("Impossible code path reached.")
            }
            val propValue = c.getText
            propName match {
              case ALIGNMENT => annotation.format.setAlignment(propValue)
              case ALIGNMENT_UNITS => annotation.format.setAlignmentUnits(propValue)
              case BINARY_BOOLEAN_TRUE_REP => annotation.format.setBinaryBooleanTrueRep(propValue)
              case BINARY_BOOLEAN_FALSE_REP => annotation.format.setBinaryBooleanFalseRep(propValue)
              case BINARY_FLOAT_REPRESENTATION => annotation.format.setFloatRepresentation(propValue)
              case BINARY_NUMBER_REPRESENTATION => annotation.format.setBinaryNumberRepresentation(propValue)
              case BYTE_ORDER => annotation.format.setByteOrder(propValue)
              case CALENDAR_PATTERN => annotation.format.setCalendarPattern(propValue)
              case CALENDAR_PATTERN_KIND => annotation.format.setCalendarPatternKind(propValue)
              case CALENDAR_TIME_ZONE => annotation.format.setCalendarTimeZone(propValue)
              case DEFAULT => annotation.format.setDefault(propValue)
              case ESCAPE_KIND => annotation.format.setEscapeKind(propValue)
              case ESCAPE_BLOCK_START => annotation.format.setEscapeBlockStart(propValue)
              case ESCAPE_BLOCK_END => annotation.format.setEscapeBlockEnd(propValue)
              case ESCAPE_CHARACTER => annotation.format.setEscapeCharacter(propValue)
              case ESCAPE_ESCAPE_CHARACTER => annotation.format.setEscapeEscapeCharacter(propValue)
              case ESCAPE_SCHEMA_REF =>
                annotation.format += definitions(propValue)
              case ENCODING => annotation.format.setEncoding(propValue)
              case FINAL_TERMINATOR_CAN_BE_MISSING => annotation.format.setFinalTerminatorCanBeMissing(propValue)
              case IGNORE_CASE => annotation.format.setIgnoreCase(propValue)
              case INITIATED_CONTENT => annotation.format.setInitiatedContent(propValue)
              case INITIATOR => annotation.format.setInitiator(propValue)
              case INPUT_VALUE_CALC =>
                annotation inputValue = new InputValue(); annotation.inputValue expression = propValue
              case LEADING_SKIP_BYTES => annotation.format.setLeadingSkipBytes(propValue)
              case LENGTH_PATTERN => annotation.format.setLengthPattern(unescapePattern(propValue))
              case LENGTH_KIND | CHOICE_LENGTH_KIND => annotation.format.setLengthKind(propValue)
              case LENGTH | CHOICE_LENGTH => annotation.format.setLength(propValue)
              case MAX_LENGTH => annotation.format.setLength(propValue)
              case MAX_OCCURS => annotation.format.setMaxOccurs(propValue)
              case MIN_OCCURS => annotation.format.setMinOccurs(propValue)
              case NUMBER_DECIMAL_SEPARATOR => annotation.format.setDecimalSeparator(propValue)
              case OCCURS_COUNT_KIND => annotation.format.setOccursCountKind(propValue)
              case OCCURS_COUNT => annotation.format.setOccursCount(propValue)
              case OCCURS_STOP_VALUE => annotation.format.setOccursStopValue(propValue)
              case PREFIX_LENGTH_TYPE => annotation.format setPrefixLengthType (propValue)
              case PREFIX_INCLUDES_PREFIX_LENGTH => annotation.format setPrefixIncludesPrefixLength (propValue)
              case REF => annotation.format += definitions(propValue)
              case REPRESENTATION => annotation.format.setRepresentation(propValue)
              case SEPARATOR => annotation.format.setSeparator(propValue)
              case SEPARATOR_POSITION => annotation.format.setSeparatorPosition(propValue)
              case SEPARATOR_POLICY => annotation.format.setSeparatorPolicy(propValue)
              case SEQUENCE_KIND => annotation.format.setSequenceKind(propValue)
              case STOP_VALUE => annotation.format.setStopValue(propValue)
              case TERMINATOR => annotation.format.setTerminator(propValue)
              //            case TEXT_CALENDAR_FORMAT_REF =>
              //              annotation.format += definitions(propValue)
              case TEXT_BOOLEAN_TRUE_REP => annotation.format.setTextBooleanTrueRep(propValue)
              case TEXT_BOOLEAN_FALSE_REP => annotation.format.setTextBooleanFalseRep(propValue)
              case TEXT_NUMBER_BASE => annotation.format.setBase(propValue)
              case TEXT_NUMBER_CHECK_POLICY => annotation.format.setTextNumberCheckPolicy(propValue)
              case TEXT_NUMBER_PATTERN => annotation.format.setTextNumberPattern(propValue)
              case TEXT_NUMBER_ROUNDING_MODE => annotation.format.setTextNumberRoundingMode(propValue)
              case TEXT_STANDARD_DECIMAL_SEPARATOR => annotation.format.setTextStandardDecimalSeparator(propValue)
              case TEXT_STANDARD_EXPONENT_CHARACTER => annotation.format.setTextStandardExponentCharacter(propValue)
              case TEXT_STANDARD_INFINITY_REP => annotation.format.setTextStandardInfinityRep(propValue)
              case TEXT_STANDARD_GROUPING_SEPARATOR => annotation.format.setTextStandardGroupingSeparator(propValue)
              case TEXT_STANDARD_NAN_REP => annotation.format.setTextStandardNanRep(propValue)
              case TEXT_STANDARD_ZERO_REP => annotation.format.setTextStandardZeroRep(propValue)
              case TEXT_ZONED_SIGN_STYLE => annotation.format.setTextZonedSignStyle(propValue)
              case TEXT_STANDARD_BASE => annotation.format.setBase(propValue)
              //           case TEXT_NUMBER_FORMAT_REF =>
              //              annotation.format += definitions(propValue)
              case TEXT_STRING_JUSTIFICATION | TEXT_NUMBER_JUSTIFICATION |
                TEXT_CALENDAR_JUSTIFICATION | TEXT_BOOLEAN_JUSTIFICATION =>
                annotation.format.setTextStringJustification(propValue)
              case TEXT_STRING_PAD_CHARACTER | TEXT_NUMBER_PAD_CHARACTER |
                TEXT_CALENDAR_PAD_CHARACTER | TEXT_BOOLEAN_PAD_CHARACTER =>
                annotation.format.setPadCharacter(propValue)
              case TEXT_NUMBER_REP => annotation.format.setTextNumberRep(propValue)
              case TRAILING_SKIP_BYTES => annotation.format.setTrailingSkipBytes(propValue)
              case _ => throw new DFDLSchemaDefinitionException("Unknown property " + propName + " with value " + propValue, schemaContext = c)
            }
          }
          case _ =>
        }
      } catch {
        case e: IllegalArgumentException => throw new DFDLSchemaDefinitionException(e.getMessage, cause = e, schemaContext = c)
      }
  }

  private def parseAttributes(attributes:Iterable[Attribute],parent:Element,
                              annotation:Annotation,definitions:Map[String,Format],parseRef:Boolean):Unit = {
    try {
      for(a <- attributes)
        parseAttribute(a,parent,annotation,definitions,parseRef)
    }catch {
      case e:IllegalArgumentException => throw new DFDLSchemaDefinitionException(e.getMessage,cause = e,schemaContext = parent)
      case e:NumberFormatException => throw new DFDLSchemaDefinitionException(e.getMessage,cause = e,schemaContext = parent)
    }
  }
  
  //TODO: All of this property assignment stuff would be better done reflectively rather than having to maintain 
  // all this code that is isomorphic to the list of property names.
  
  //TODO: refactor so there is one common assignment thing, not one for element form, and one for attribute form.
  // and... that one thing should work reflectively.

  private def parseAttribute(a:Attribute,parent:Element,
                             annotation:Annotation,definitions:Map[String,Format],parseRef:Boolean):Unit = {

    //a getValue -> unescape(a getValue)

    getFullName(a) match {
      case TEXT_NUMBER_BASE | TEXT_NUMBER_BASE_PREFIXED =>
        annotation.format.setBase(a getValue)
      case TEXT_NUMBER_CHECK_POLICY | TEXT_NUMBER_CHECK_POLICY_PREFIXED =>
        annotation.format.setTextNumberCheckPolicy(a getValue)
      case TEXT_NUMBER_PATTERN | TEXT_NUMBER_PATTERN_PREFIXED =>
        annotation.format.setTextNumberPattern(a getValue)
      case TEXT_NUMBER_ROUNDING_MODE | TEXT_NUMBER_ROUNDING_MODE_PREFIXED =>
        annotation.format.setTextNumberRoundingMode(a getValue)
      case TEXT_STANDARD_DECIMAL_SEPARATOR | TEXT_STANDARD_DECIMAL_SEPARATOR_PREFIXED =>
        annotation.format.setTextStandardDecimalSeparator(a getValue)
      case TEXT_STANDARD_EXPONENT_CHARACTER | TEXT_STANDARD_EXPONENT_CHARACTER_PREFIXED =>
        annotation.format.setTextStandardExponentCharacter(a getValue)
      case TEXT_STANDARD_INFINITY_REP | TEXT_STANDARD_INFINITY_REP_PREFIXED =>
        annotation.format.setTextStandardInfinityRep(a getValue)
      case TEXT_STANDARD_GROUPING_SEPARATOR | TEXT_STANDARD_GROUPING_SEPARATOR_PREFIXED =>
        annotation.format.setTextStandardGroupingSeparator(a getValue)
      case TEXT_STANDARD_NAN_REP | TEXT_STANDARD_NAN_REP_PREFIXED =>
        annotation.format.setTextStandardNanRep(a getValue)
      case TEXT_STANDARD_ZERO_REP | TEXT_STANDARD_ZERO_REP_PREFIXED =>
        annotation.format.setTextStandardZeroRep(a getValue)
      case TEXT_ZONED_SIGN_STYLE | TEXT_ZONED_SIGN_STYLE_PREFIXED =>
        annotation.format.setTextZonedSignStyle(a getValue)
//      case TEXT_NUMBER_FORMAT_REF | TEXT_NUMBER_FORMAT_REF_PREFIXED =>
//        annotation.format += definitions(a getValue)
//      case TEXT_CALENDAR_FORMAT_REF | TEXT_CALENDAR_FORMAT_REF_PREFIXED =>
//        annotation.format += definitions(a getValue)
      case TEXT_STRING_JUSTIFICATION | TEXT_NUMBER_JUSTIFICATION |
              TEXT_CALENDAR_JUSTIFICATION | TEXT_BOOLEAN_JUSTIFICATION |
              TEXT_STRING_JUSTIFICATION_PREFIXED | TEXT_NUMBER_JUSTIFICATION_PREFIXED |
              TEXT_CALENDAR_JUSTIFICATION_PREFIXED | TEXT_BOOLEAN_JUSTIFICATION_PREFIXED =>
        annotation.format setTextStringJustification(a getValue)
      case TEXT_STRING_PAD_CHARACTER |TEXT_NUMBER_PAD_CHARACTER |
              TEXT_CALENDAR_PAD_CHARACTER | TEXT_BOOLEAN_PAD_CHARACTER |
              TEXT_STRING_PAD_CHARACTER_PREFIXED |TEXT_NUMBER_PAD_CHARACTER_PREFIXED |
              TEXT_CALENDAR_PAD_CHARACTER_PREFIXED | TEXT_BOOLEAN_PAD_CHARACTER_PREFIXED =>
        annotation.format.setPadCharacter(a getValue)
      case TEXT_NUMBER_REP | TEXT_NUMBER_REP_PREFIXED =>
        annotation.format.setTextNumberRep(a getValue)
      case TRAILING_SKIP_BYTES | TRAILING_SKIP_BYTES_PREFIXED =>
        annotation.format.setTrailingSkipBytes(a getValue)
      case "type" => annotation.format setTypeName(getFullName(a getValue,getNamespaces(parent))) 
      //splitting this match in 2 because the compiler tends to run out of memory :(
      case _ => parseAttribute2(a,parent,annotation,definitions,parseRef)
    }
  }

  private def parseAttribute2(a:Attribute,parent:Element,
                             annotation:Annotation,definitions:Map[String,Format],parseRef:Boolean):Unit = {
    getFullName(a) match {
      case ALIGNMENT | ALIGNMENT_PREFIXED => annotation.format.setAlignment(a getValue)
      case ALIGNMENT_UNITS | ALIGNMENT_UNITS_PREFIXED => annotation.format.setAlignmentUnits(a getValue)
      case BINARY_BOOLEAN_FALSE_REP | BINARY_BOOLEAN_FALSE_REP_PREFIXED => annotation.format.setBinaryBooleanFalseRep(a getValue)
      case BINARY_BOOLEAN_TRUE_REP | BINARY_BOOLEAN_TRUE_REP_PREFIXED => annotation.format.setBinaryBooleanTrueRep(a getValue)
      case BINARY_FLOAT_REPRESENTATION | BINARY_FLOAT_REPRESENTATION_PREFIXED =>
        annotation.format setFloatRepresentation(a getValue)
      case BINARY_NUMBER_REPRESENTATION | BINARY_NUMBER_REPRESENTATION_PREFIXED =>
        annotation.format setBinaryNumberRepresentation(a getValue)
      case BYTE_ORDER | BYTE_ORDER_PREFIXED =>
        annotation.format setByteOrder(a getValue)
      case CALENDAR_PATTERN | CALENDAR_PATTERN_PREFIXED =>
        annotation.format setCalendarPattern(a getValue)
      case CALENDAR_PATTERN_KIND | CALENDAR_PATTERN_KIND_PREFIXED =>
        annotation.format setCalendarPatternKind(a getValue)
      case CALENDAR_TIME_ZONE | CALENDAR_TIME_ZONE_PREFIXED =>
        annotation.format setCalendarTimeZone(a getValue)
      case DEFAULT | DEFAULT_PREFIXED => annotation.format setDefault(a getValue)
      case ESCAPE_KIND | ESCAPE_KIND_PREFIXED => annotation.format.setEscapeKind(a getValue)
      case ESCAPE_BLOCK_START | ESCAPE_BLOCK_START_PREFIXED => annotation.format.setEscapeBlockStart(a getValue)
      case ESCAPE_BLOCK_END | ESCAPE_BLOCK_END_PREFIXED => annotation.format.setEscapeBlockEnd(a getValue)
      case ESCAPE_CHARACTER | ESCAPE_CHARACTER_PREFIXED => annotation.format.setEscapeCharacter(a getValue)
      case ESCAPE_ESCAPE_CHARACTER | ESCAPE_ESCAPE_CHARACTER_PREFIXED =>
        annotation.format.setEscapeEscapeCharacter(a getValue)
      case ESCAPE_SCHEMA_REF | ESCAPE_SCHEMA_REF_PREFIXED =>
        annotation.format += definitions(a getValue)
      case ENCODING | ENCODING_PREFIXED =>
        annotation.format setEncoding(a getValue)
      case FINAL_TERMINATOR_CAN_BE_MISSING | FINAL_TERMINATOR_CAN_BE_MISSING_PREFIXED =>
        annotation.format setFinalTerminatorCanBeMissing(a getValue)
      case IGNORE_CASE | IGNORE_CASE_PREFIXED =>
        annotation.format setIgnoreCase(a getValue)
      case INITIATED_CONTENT | INITIATED_CONTENT_PREFIXED =>
        annotation.format setInitiatedContent(a getValue)
      case INITIATOR | INITIATOR_PREFIXED =>
        annotation.format setInitiator(a getValue)
      case INPUT_VALUE_CALC | INPUT_VALUE_CALC_PREFIXED =>
        annotation inputValue = new InputValue(); annotation.inputValue expression = a getValue
      case LEADING_SKIP_BYTES | LEADING_SKIP_BYTES_PREFIXED => annotation.format.setLeadingSkipBytes(a getValue)
      case LENGTH | LENGTH_PREFIXED | CHOICE_LENGTH | CHOICE_LENGTH_PREFIXED =>
        annotation.format setLength(a getValue)
      case LENGTH_PATTERN | LENGTH_PATTERN_PREFIXED =>
        annotation.format setLengthPattern(unescapePattern(a getValue))
      case LENGTH_KIND | LENGTH_KIND_PREFIXED | CHOICE_LENGTH_KIND | CHOICE_LENGTH_KIND_PREFIXED =>
        annotation.format setLengthKind(a getValue)
      case MAX_LENGTH | MAX_LENGTH_PREFIXED =>
        annotation.format setLength(a getValue)
      case MAX_OCCURS | MAX_OCCURS_PREFIXED =>
        annotation.format setMaxOccurs(a getValue)
      case MIN_OCCURS | MIN_OCCURS_PREFIXED =>
        annotation.format setMinOccurs(a getValue)
      case "name" =>
      case NILLABLE =>
      	if(a.getValue == "true") throw new UnimplementedException("nillable='true' not implemented", schemaContext = parent) 
      case NUMBER_DECIMAL_SEPARATOR | NUMBER_DECIMAL_SEPARATOR_PREFIXED =>
        annotation.format setDecimalSeparator(a getValue)
      case OCCURS_COUNT | OCCURS_COUNT_PREFIXED =>
        annotation.format setOccursCount(a getValue)
      case OCCURS_COUNT_KIND | OCCURS_COUNT_KIND_PREFIXED =>
        annotation.format setOccursCountKind(a getValue)
      case OCCURS_STOP_VALUE | OCCURS_STOP_VALUE_PREFIXED =>
        annotation.format setOccursStopValue(a getValue)
      case REF | REF_PREFIXED => if (parseRef) annotation.format += definitions(a getValue)
      case REPRESENTATION | REPRESENTATION_PREFIXED =>
        annotation.format setRepresentation(a getValue)
      case PREFIX_LENGTH_TYPE | PREFIX_LENGTH_TYPE_PREFIXED =>
        annotation.format setPrefixLengthType(a getValue)
      case PREFIX_INCLUDES_PREFIX_LENGTH | PREFIX_INCLUDES_PREFIX_LENGTH_PREFIXED =>
        annotation.format setPrefixIncludesPrefixLength(a getValue)
      case SEPARATOR | SEPARATOR_PREFIXED =>
        annotation.format setSeparator(a getValue)
      case SEPARATOR_POSITION | SEPARATOR_POSITION_PREFIXED =>
        annotation.format setSeparatorPosition(a getValue)
      case SEPARATOR_POLICY | SEPARATOR_POLICY_PREFIXED =>
        annotation.format setSeparatorPolicy(a getValue)
      case SEQUENCE_KIND | SEQUENCE_KIND_PREFIXED => annotation.format.setSequenceKind(a getValue)
      case STOP_VALUE | STOP_VALUE_PREFIXED =>
        annotation.format setStopValue(a getValue)
      case TERMINATOR | TERMINATOR_PREFIXED =>
        annotation.format setTerminator(a getValue)
      case TEXT_STANDARD_BASE | TEXT_STANDARD_BASE_PREFIXED =>
        annotation.format setBase(a getValue)
      case TEXT_BOOLEAN_TRUE_REP | TEXT_BOOLEAN_TRUE_REP_PREFIXED =>
        annotation.format setTextBooleanTrueRep(a getValue)
      case TEXT_BOOLEAN_FALSE_REP | TEXT_BOOLEAN_FALSE_REP_PREFIXED =>
        annotation.format setTextBooleanFalseRep(a getValue)
      case _ => if (getFullName(a).startsWith(DFDL_NAMESPACE))
        throw new DFDLSchemaDefinitionException("Unknown property "+a.getName,schemaContext = parent)
    }
  }

  private def parseVariableDefinition(annotation:Annotation,node:Element) = {
    val name = getAttribute(node,"name") match {
      case Some(s) => s
      case None => getAttribute(node,"ref") match {
        case Some(s) => s
        case None => throw new DFDLSchemaDefinitionException("Variable without a name",schemaContext = node)
      }
    }

    val typeName:String = getAttribute(node,"type") match {
      case Some(s) => getFullName(s,getNamespaces(node))
      case None => null
    }

    var value = getAttribute(node,"defaultValue")

    if (value==None){
      val s = node getText; //unescape(node getText)
      if (s != "")
        value = Some(s)
    }

    annotation addVariableDefinition(new VariableDefinition(name,value,typeName))
  }

  private def parseVariableBinding(annotation:Annotation,node:Element) = {
    val name = getAttribute(node,"ref") match {
      case Some(s) => s
      case None => throw new DFDLSchemaDefinitionException("Variable without a name",schemaContext = node)
    }

    var value = getAttribute(node,"defaultValue")

    if (value==None)
      value = Some(node getText)

    annotation addVariableBinding(new VariableBinding(name,value get)) //unescape(value get)
  }

  private def parseDiscriminator(annotation:Annotation,node:Element) = {
    parseTest(annotation,node,false)
  }

  private def parseTest(annotation:Annotation,node:Element,isAssertion:Boolean) = {
    val test = getAttribute(node,"test") match {
      case Some(s) => s
      case None => node getText
    }

    val timing = getAttribute(node,"timing")


    if (isAssertion){
      val assertion = new Assertion
      assertion.test = test
      getAttribute(node,"message") match {
        case Some(s) => assertion.message = s
        case None =>
      }

      timing match {
        case Some("before") => assertion before = true
        case Some("after") => assertion before = false
        case None =>
        case x => throw new DFDLSchemaDefinitionException("Invalid value for timing in assertion:"+x,schemaContext = node)
      }
      annotation addAssertion(assertion)
    }else{
      val discrimination = new Discrimination
      discrimination.test = test
      timing match {
        case Some("before") => discrimination before = true
        case Some("after") => discrimination before = false
        case None =>
        case x => throw new DFDLSchemaDefinitionException("Invalid value for timing in discrimination:"+x,schemaContext = node)
      }
      annotation setDiscriminator(discrimination)
    }
  }

  private def parseAssertion(annotation:Annotation,node:Element) = {
    parseTest(annotation,node,true)
  }


  /**
   * Translates a string with DFDL entity characters an DFDL class characters into a pattern
   */
  def unescape(s:String):Regex = {
    val regex = new Regex
    val escape = new StringBuilder
    var state = 0
    for(c <- s) state match {
      case 0 =>
        if (c=='%'){
          state = 1
          escape setLength(0)
        }else
          regex addAlternatives (List(c.toString),false,false)
      case 1 =>
        if (c=='%') {
          state = 0
          regex addAlternatives (List(c.toString),false,false)
        } else if (c==';'){
          throw new IllegalArgumentException("Empty character entity")
        } else{
          escape setLength(0)
          escape append(c)
          state = 2
        }
      case 2 =>
        if (c==';'){
          mapEscape(regex,escape toString)
          state = 0
        }else
          escape append(c)
    }
    regex
  }

  
  //TODO FIXME decimal values, hexadecimal values
  private def mapEscape(regex:Regex,s:String) =
    if (s startsWith("#x")){
      throw new UnimplementedException("hexadecimal values in escape characters") // TODO implement
    }else if (s startsWith("#")){
      throw new UnimplementedException("decimal values in escape characters") // TODO implement
    }else s match {
      case "NUL" => regex addAlternatives (List("\u0000"),false,false)
      case "SOH" => regex addAlternatives (List("\u0001"),false,false)
      case "STX" => regex addAlternatives (List("\u0002"),false,false)
      case "ETX" => regex addAlternatives (List("\u0003"),false,false)
      case "EOT" => regex addAlternatives (List("\u0004"),false,false)
      case "ENQ" => regex addAlternatives (List("\u0005"),false,false)
      case "ACK" => regex addAlternatives (List("\u0006"),false,false)
      case "BEL" => regex addAlternatives (List("\u0007"),false,false)
      case "BS" => regex addAlternatives (List("\u0008"),false,false)
      case "HT" => regex addAlternatives (List("\u0009"),false,false)
      case "LF" => regex addAlternatives (List("\u000A"),false,false)
      case "VT" => regex addAlternatives (List("\u000B"),false,false)
      case "FF" => regex addAlternatives (List("\u000C"),false,false)
      case "CR" => regex addAlternatives (List("\u000D"),false,false)
      case "SO" => regex addAlternatives (List("\u000E"),false,false)
      case "SI" => regex addAlternatives (List("\u000F"),false,false)
      case "DLE" => regex addAlternatives (List("\u0010"),false,false)
      case "DC1" => regex addAlternatives (List("\u0011"),false,false)
      case "DC2" => regex addAlternatives (List("\u0012"),false,false)
      case "DC3" => regex addAlternatives (List("\u0013"),false,false)
      case "DC4" => regex addAlternatives (List("\u0014"),false,false)
      case "NAK" => regex addAlternatives (List("\u0015"),false,false)
      case "SYN" => regex addAlternatives (List("\u0016"),false,false)
      case "ETB" => regex addAlternatives (List("\u0017"),false,false)
      case "CAN" => regex addAlternatives (List("\u0018"),false,false)
      case "EM" => regex addAlternatives (List("\u0019"),false,false)
      case "SUB" => regex addAlternatives (List("\u001A"),false,false)
      case "ESC" => regex addAlternatives (List("\u001B"),false,false)
      case "FS" => regex addAlternatives (List("\u001C"),false,false)
      case "GS" => regex addAlternatives (List("\u001D"),false,false)
      case "RS" => regex addAlternatives (List("\u001E"),false,false)
      case "US" => regex addAlternatives (List("\u001F"),false,false)
      case "SP" => regex addAlternatives (List("\u0020"),false,false)
      case "DEL" => regex addAlternatives (List("\u007F"),false,false)
      case "NBSP" => regex addAlternatives (List("\u00A0"),false,false)
      case "NEL" => regex addAlternatives (List("\u0085"),false,false)
      case "LS" => regex addAlternatives (List("\u2028"),false,false)
      
      case "NL" => regex addAlternatives (List("\u000A","\u000D","\u000D\u000A","\u0085","\u2028"),false,false)
      case "WSP" => regex addAlternatives (List("\u0009","\u000A","\u000B","\u000C","\u000D","\u0020",
        "\u0085","\u00A0","\u1680","\u180E","\u2000","\u2001","\u2002","\u2003","\u2004","\u2005",
        "\u2006","\u2007","\u2008","\u2009","\u200A","\u2028","\u2029","\u202F","\u205F","\u3000"),false,false)
      case "WSP*" => regex addAlternatives (List("\u0009","\u000A","\u000B","\u000C","\u000D","\u0020",
        "\u0085","\u00A0","\u1680","\u180E","\u2000","\u2001","\u2002","\u2003","\u2004","\u2005",
        "\u2006","\u2007","\u2008","\u2009","\u200A","\u2028","\u2029","\u202F","\u205F","\u3000"),true,true)
      case "WSP+" => regex addAlternatives (List("\u0009","\u000A","\u000B","\u000C","\u000D","\u0020",
        "\u0085","\u00A0","\u1680","\u180E","\u2000","\u2001","\u2002","\u2003","\u2004","\u2005",
        "\u2006","\u2007","\u2008","\u2009","\u200A","\u2028","\u2029","\u202F","\u205F","\u3000"),false,true)
      case _ => throw new IllegalArgumentException("Unknwon escape character '"+s+"'")
    }

  /**
   * Takes a string with a Java regular expression and substitutes DFDL class and escape characters with their
   * corresponding regular expressions
   * 
   */
  def unescapePattern(s:String):String = {
    val sb = new StringBuilder()
    val escape = new StringBuilder
    var state = 0
    for(c <- s) state match {
      case 0 =>
        if (c=='%'){
          state = 1
          escape setLength(0)
        }else
          sb.append(c)
      case 1 =>
        if (c=='%') {
          state = 0
          sb.append(c)
        } else if (c==';'){
          throw new DFDLSchemaDefinitionException("Empty character entity")
        } else{
          escape setLength(0)
          escape append(c)
          state = 2
        }
      case 2 =>
        if (c==';')
          mapEscapePattern(escape toString) match {
            case Some(e) => sb append(e); state = 0
            case None => throw new IllegalArgumentException("Unknown character entity '"+escape.toString+"'")
          }
        else
          escape append(c)
    }
    sb toString
  }


  //TODO FIXME decimal values, hexadecimal values, character classes
  private def mapEscapePattern(s:String):Option[String] =
    if (s startsWith("#x")){
      None
    }else if (s startsWith("#")){
      None
    }else s match {
      case "NUL" => Some("\u0000")
      case "SOH" => Some("\u0001")
      case "STX" => Some("\u0002")
      case "ETX" => Some("\u0003")
      case "EOT" => Some("\u0004")
      case "ENQ" => Some("\u0005")
      case "ACK" => Some("\u0006")
      case "BEL" => Some("\u0007")
      case "BS" => Some("\u0008")
      case "HT" => Some("\u0009")
      case "LF" => Some("\u000A")
      case "VT" => Some("\u000B")
      case "FF" => Some("\u000C")
      case "CR" => Some("\u000D")
      case "SO" => Some("\u000E")
      case "SI" => Some("\u000F")
      case "DLE" => Some("\u0010")
      case "DC1" => Some("\u0011")
      case "DC2" => Some("\u0012")
      case "DC3" => Some("\u0013")
      case "DC4" => Some("\u0014")
      case "NAK" => Some("\u0015")
      case "SYN" => Some("\u0016")
      case "ETB" => Some("\u0017")
      case "CAN" => Some("\u0018")
      case "EM" => Some("\u0019")
      case "SUB" => Some("\u001A")
      case "ESC" => Some("\u001B")
      case "FS" => Some("\u001C")
      case "GS" => Some("\u001D")
      case "RS" => Some("\u001E")
      case "US" => Some("\u001F")
      case "SP" => Some("\u0020")
      case "DEL" => Some("\u007F")
      case "NBSP" => Some("\u00A0")
      case "NEL" => Some("\u0085")
      case "LS" => Some("\u2028")
      
      case "NL" => Some("(:?\u000A|\u000D|\u000D\u000A|\u0085|\u2028)")
      case "WSP" => Some("(:?[\u0009-\u000D]|\u0020|\u0085|\u00A0|\u1680|\u180E|" +
              "[u2000-u200A]|\u2028|\u2029|\u202F|\u205F|\u3000)")
      case "WSP*" => Some("(:?[\u0009-\u000D]|\u0020|\u0085|\u00A0|\u1680|\u180E|" +
              "[u2000-u200A]|\u2028|\u2029|\u202F|\u205F|\u3000)*")
      case "WSP+" => Some("(:?[\u0009-\u000D]|\u0020|\u0085|\u00A0|\u1680|\u180E|" +
              "[u2000-u200A]|\u2028|\u2029|\u202F|\u205F|\u3000)+")
      case _ => None
    }

  /**
   * Returns a list of whitespace separated tokens from a string
   */
  def separate(s:String):List[String] = {
    s.split(" ").toList
//    var strings:List[String] = Nil
//    var state = 0
//    var sb:StringBuilder = null
//
//    for(c <- s){
//      state match {
//        case 0 => if (c==0) { state = 2; sb = new StringBuilder }
//        else if (c!=' ') { state = 1; sb = new StringBuilder; sb.append(c) }
//        case 1 => if (c==' ') { state = 0; strings = sb.toString :: strings }
//        else if (c==0) { state = 2; strings = sb.toString :: strings; sb = new StringBuilder }
//        else { sb.append(c) }
//        case 2 => if (c==0) { state = 0; strings = sb.toString :: strings }
//        else { sb.append(c) }
//      }
//    }
//
//    state match {
//      case 1 => strings = sb.toString :: strings
//      case 2 => throw new DFDLSchemaDefinitionException("unmatched ' in "+s,null,null,null,None)
//      case _ =>
//    }
  }


}
