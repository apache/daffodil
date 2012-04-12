package daffodil.processors

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

import java.nio.charset.Charset

import daffodil.debugger.DebuggingListener
import daffodil.exceptions.UnimplementedException
import daffodil.exceptions.DFDLSchemaDefinitionException
import daffodil.xml.XMLUtil

import daffodil.processors.input.{DebuggingBasicProcessor, InputExpressionProcessor, BasicProcessor}
import daffodil.processors.input.binary.{BooleanBinaryProcessor, FloatBinaryProcessor, DecimalBinaryProcessor, BinaryProcessor}
import daffodil.processors.internal._
import daffodil.schema.annotation._
import enumerations._
import daffodil.processors.input.text._

/**
 * Instantiate BasicProcessors and InternalProcessors based on annotations
 *
 * @see daffodil.processors.input.BasicProcessor
 * @see daffodil.processors.internal.InternalProcessor
 *
 * @version 1
 * @author Alejandro Rodriguez
 *
 */
object ProcessorFactory{
  
  private var debugger:DebuggingListener = null

  /**
   * Instantiate a BasicProcessor
   *
   * @param annotation the DFDL annotation describing the physical format to be processed by the processor
   */
  def getInputProcessor(annotation:Annotation):BasicProcessor =
    if (annotation.inputValue != null)
      wrapDebugger(annotation,new InputExpressionProcessor(annotation.inputValue.expression,annotation.format.typeName))
    else
      wrapDebugger(annotation,getRepresentation(annotation))


  /**
   * Whether the annotation describes a hidden element
   *
   */
  def isHidden(x:Annotation):Boolean = 
    x.hidden != null

  /**
   * Instantiate the InternalProcessors with BEFORE timing described the the annotation
   */
  def getBeforeProcessor(annotation:Annotation):List[InternalProcessor] = {
    var processors:List[InternalProcessor] = Nil
    
    processors = processors :::  
    annotation.variableDefinitions.map { x:VariableDefinition => 
      wrapDebugger(annotation,new DefineVariable(x name,x typeName,x value)) }
    
    processors = processors ::: 
    annotation.variableBindings.map { x:VariableBinding => 
      wrapDebugger(annotation,new SetVariable(x name,x value)) }
    
    processors = processors ::: 
    annotation.assertions.filter { _ before }.map { x:Assertion => 
      wrapDebugger(annotation,new Assert(x test, x message)) }
    
    if (annotation.discriminator != null && annotation.discriminator.before)
      processors = processors ::: List(
    	wrapDebugger(annotation,new Discriminator(annotation.discriminator.test)) )
    
    processors  
  }

  /**
   * Instantiate the InternalProcessors with AFTER timing described the the annotation
   */
  def getAfterProcessor(annotation:Annotation):List[InternalProcessor] = {
    val processors = annotation.assertions.filter { x:Assertion => ! x.before }.map { 
      x:Assertion => wrapDebugger(annotation,new Assert(x test, x message)) }
    if (annotation.discriminator != null && ! annotation.discriminator.before)
      processors ::: List(wrapDebugger(annotation,new Discriminator(annotation.discriminator.test)) )
    else
      processors
  }

  
  private def getRepresentation(x:Annotation):BasicProcessor =
    x.format.representation match { 
      case Some(Text) => getTextRepresentation(x)
      case Some(Binary) => getBinaryRepresentation(x)
      case None  =>  getTextRepresentation(x)
    }	      
  
  
  private def getTextRepresentation(x:Annotation):TextProcessor = {

    val encoding = x.format.encoding match {
      case Some(e) => e
      case None => AnnotationDefaults defaultEncoding
    }
    
    val textProcessor = x.format.typeName match {
      case Some(XMLUtil.XSD_STRING) => new TextProcessor(encoding,true)
      case Some(XMLUtil.XSD_INT) | Some(XMLUtil.XSD_INTEGER) |
      Some(XMLUtil.XSD_LONG) | Some(XMLUtil.XSD_SHORT) | 
      Some(XMLUtil.XSD_BYTE) | Some(XMLUtil.XSD_UNSIGNED_LONG) |
      Some(XMLUtil.XSD_UNSIGNED_INT) | Some(XMLUtil.XSD_NON_NEGATIVE_INTEGER) | Some(XMLUtil.XSD_UNSIGNED_SHORT) |
      Some(XMLUtil.XSD_UNSIGNED_BYTE) => {
        
        val intProcessor = new NumberTextProcessor(encoding,true)
        intProcessor.setTextNumberRep(x.format.textNumberRep.getOrElse(AnnotationDefaults defaultTextNumberRep))
        intProcessor.setBase(x.format.base.getOrElse(AnnotationDefaults defaultBase))
        x.format.textNumberPattern.map { intProcessor.setTextNumberPattern(_) }
        x.format.textStandardDecimalSeparator.map { intProcessor.setTextStandardDecimalSeparator(_) }
        x.format.textStandardGroupingSeparator.map { intProcessor.setTextStandardGroupingSeparator(_) }
        x.format.textStandardExponentCharacter.map { intProcessor.setTextStandardExponentCharacter(_) }
        intProcessor.setTextNumberCheckPolicy(x.format.textNumberCheckPolicy.getOrElse(AnnotationDefaults defaultTextNumberCheckPolicy))
        x.format.textStandardZeroRep  match {
           case EmptyValue =>
           case e => intProcessor.setTextStandardZeroRep(e)
        }
        
        intProcessor
      }          
      case Some(XMLUtil.XSD_FLOAT) | Some(XMLUtil.XSD_DOUBLE) | Some(XMLUtil.XSD_DECIMAL)=> {
        val floatProcessor = new NumberTextProcessor(encoding,true)
        
        x.format.textStandardInfinityRep.map { floatProcessor.setTextStandardInfinityRep(_) }
        x.format.textStandardNanRep.map { floatProcessor.setTextStandardNanRep(_) }
        floatProcessor.setTextNumberRep(x.format.textNumberRep.getOrElse(AnnotationDefaults defaultTextNumberRep))
        floatProcessor.setBase(x.format.base.getOrElse(AnnotationDefaults defaultBase))
        x.format.textNumberPattern.map { floatProcessor.setTextNumberPattern(_) }
        x.format.textStandardDecimalSeparator.map { floatProcessor.setTextStandardDecimalSeparator(_) }
        x.format.textStandardGroupingSeparator.map { floatProcessor.setTextStandardGroupingSeparator(_) }
        x.format.textStandardExponentCharacter.map { floatProcessor.setTextStandardExponentCharacter(_) }
        floatProcessor.setTextNumberCheckPolicy(x.format.textNumberCheckPolicy.getOrElse(AnnotationDefaults defaultTextNumberCheckPolicy))
        x.format.textStandardZeroRep  match {
           case EmptyValue =>
           case e => floatProcessor.setTextStandardZeroRep(e)
        }
        
        floatProcessor
      }
      case Some(XMLUtil.XSD_DATE) | Some(XMLUtil.XSD_DATE_TIME) | Some(XMLUtil.XSD_TIME) => {
        val dateProcessor = new CalendarTextProcessor(encoding,true)
        val patternKind = x.format.calendarPatternKind.getOrElse(AnnotationDefaults defaultCalendarPatternKind)

        val pattern = x.format.calendarPattern match {
          case Some(p) => p
          case None =>
            if (patternKind == ExplicitCalendar)
              throw new DFDLSchemaDefinitionException("Missing calendarPattern property",schemaContext = x element)
            else
              null
        }
        val timeZone = x.format.calendarTimeZone match {
          case Some(tz) => tz
          case None => null
        }
        val typeName = x.format.typeName get
        
        dateProcessor setPatternKind(patternKind)
        dateProcessor setPattern(pattern)
        dateProcessor setTimeZone(timeZone)
        dateProcessor setTypeName(typeName)
        
        dateProcessor
      }

      case Some(XMLUtil.XSD_BOOLEAN) => {
        val booleanProcessor = new BooleanTextProcessor(encoding,true)

        booleanProcessor setTrueRepresentation (x.format.textBooleanTrueRep)
        booleanProcessor setFalseRepresentation (x.format.textBooleanFalseRep)
        
        booleanProcessor
      }
      
      case Some(XMLUtil.XSD_HEX_BINARY)  =>
        throw new DFDLSchemaDefinitionException("hexBinary text representation not allowed, use binary",schemaContext = x element)
      case _ => new TextProcessor(encoding,true) 
    }
    
    textProcessor.setLengthKind(x.format.lengthKind.getOrElse(AnnotationDefaults defaultLengthKind))

    if (x.format.lengthKind.getOrElse(AnnotationDefaults defaultLengthKind)==EndOfParent)
      textProcessor setEndOfParent (true)

    x.format.length match {
      case Some(l) => textProcessor.setLength(l)
      case None => if (x.format.lengthKind == Some(Explicit))
        throw new DFDLSchemaDefinitionException("Missing length property",schemaContext = x element)
    }
    
    x.format.lengthPattern match {
      case Some(l) => textProcessor.setLengthPattern(l)
      case None => if (x.format.lengthKind == Some(Pattern))
        throw new DFDLSchemaDefinitionException("Missing lengthPattern property",schemaContext = x element)
    }

    x.format.separator match {
      case EmptyValue =>
      case e => textProcessor.setSeparator(e)
    }

    textProcessor.setSeparatorPolicy(x.format.separatorPolicy.
            getOrElse(AnnotationDefaults defaultSeparatorPolicy))

    textProcessor.setSeparatorPosition(x.format.separatorPosition.getOrElse(
      AnnotationDefaults defaultSeparatorPosition))


    x.format.initiator  match {
      case EmptyValue =>
      case e => textProcessor.setInitiator(e)
    }

    x.format.terminator  match {
      case EmptyValue =>
      case e => textProcessor.setTerminator(e)
    }

    textProcessor.setFinalTerminatorCanBeMissing(x.format.documentFinalTerminatorCanBeMissing.getOrElse(
      AnnotationDefaults defaultFinalTerminatorCanBeMissing))


    textProcessor.setIgnoreCase(x.format.ignoreCase.getOrElse(AnnotationDefaults defaultIgnoreCase))

    textProcessor.setTextStringJustification(x.format.textStringJustification.getOrElse
              (AnnotationDefaults defaultTextStringJustification))

    x.format.padCharacter  match {
      case Some(t) => textProcessor.setPadCharacter(t)
      case None =>  if (x.format.textStringJustification != None && x.format.textStringJustification != Some(NoneJustification))
        throw new DFDLSchemaDefinitionException("Missing stringTextPadCharacter property",schemaContext = x element)
    }

    x.format.typeName  match {
      case Some(s) => textProcessor setTypeName(s)
      case None =>
    }

    textProcessor setEscapeKind(x.format.escapeKind.getOrElse(AnnotationDefaults defaultEscapeKind))

    x.format.escapeCharacter match {
      case Some(t) => textProcessor setEscapeCharacter(t)
      case None => if (textProcessor.getEscapeKind == EscapeCharacter)
        throw new DFDLSchemaDefinitionException("Missing escapeCharacter property",schemaContext = x element)
    }

    x.format.escapeEscapeCharacter match {
      case Some(t) => textProcessor setEscapeEscapeCharacter(t)
      case None =>
    }

    x.format.escapeBlockStart match {
      case Some(t) => textProcessor setEscapeBlockStart(t)
      case None => if (textProcessor.getEscapeKind == EscapeBlock)
        throw new DFDLSchemaDefinitionException("Missing escapeBlockStart property",schemaContext = x element)
    }

    x.format.escapeBlockEnd match {
      case Some(t) => textProcessor setEscapeBlockEnd(t)
      case None => if (textProcessor.getEscapeKind == EscapeBlock)
        throw new DFDLSchemaDefinitionException("Missing escapeBlockEnd property",schemaContext = x element)
    }


    
    textProcessor
  }
  
  private def getBinaryRepresentation(x:Annotation):BinaryProcessor = {

    val binaryProcessor = x.format.typeName match {
      case Some(XMLUtil.XSD_STRING) =>
        throw new DFDLSchemaDefinitionException("String binary representation not allowed, use text",schemaContext = x element)
      case Some(XMLUtil.XSD_INT) | Some(XMLUtil.XSD_INTEGER) |
        Some(XMLUtil.XSD_LONG) | Some(XMLUtil.XSD_SHORT) |
        Some(XMLUtil.XSD_BYTE) | Some(XMLUtil.XSD_UNSIGNED_LONG) |
        Some(XMLUtil.XSD_UNSIGNED_INT) | Some(XMLUtil.XSD_UNSIGNED_SHORT) |
        Some(XMLUtil.XSD_UNSIGNED_BYTE) | Some(XMLUtil.XSD_HEX_BINARY) | Some(XMLUtil.XSD_NON_NEGATIVE_INTEGER) =>
          new DecimalBinaryProcessor(x.format.binaryNumberRepresentation.
                  getOrElse(AnnotationDefaults.defaultBinaryNumberRepresentation))
      case Some(XMLUtil.XSD_BOOLEAN) => {
        val booleanProcessor = new BooleanBinaryProcessor(x.format.binaryNumberRepresentation.getOrElse
                  (AnnotationDefaults.defaultBinaryNumberRepresentation))

        x.format.binaryBooleanTrueRep.map { booleanProcessor.setBinaryTrueRep(_) }

        x.format.binaryBooleanFalseRep match {
          case Some(l) => booleanProcessor setBinaryFalseRep (l)
          case None => throw new DFDLSchemaDefinitionException("missing binaryBooleanFalseRep property",
            schemaContext = x element)
        }
        booleanProcessor
      }
      case Some(XMLUtil.XSD_FLOAT) | Some(XMLUtil.XSD_DOUBLE) | Some(XMLUtil.XSD_DECIMAL) =>
        new FloatBinaryProcessor(x.format.floatRepresentation.
                getOrElse(AnnotationDefaults.defaultBinaryFloatRepresentation))
      case Some(XMLUtil.XSD_DATE) | Some(XMLUtil.XSD_TIME) | Some(XMLUtil.XSD_DATE_TIME) =>
        throw new UnimplementedException("binary calendar",schemaContext = x.element) //new CalendarBinaryProcessor
      case None => throw new DFDLSchemaDefinitionException("No xsd type defined for element"
        ,schemaContext = x element)
      case Some(typename) => throw new UnimplementedException("non-built-in simple type "+typename,
        schemaContext = x.element)
    }


    binaryProcessor setByteOrder(x.format.byteOrder.getOrElse(AnnotationDefaults defaultByteOrder))

    val alignment = x.format.alignment.getOrElse(AnnotationDefaults defaultAlignment)
    
    val alignmentUnits = x.format.alignmentUnits.getOrElse(AnnotationDefaults defaultAlignmentUnits)

    binaryProcessor setAlignment(alignment,alignmentUnits)

    binaryProcessor setLeadingSkipBytes(x.format.leadingSkipBytes.getOrElse(AnnotationDefaults defaultLeadingSkipBytes))

    binaryProcessor setTrailingSkipBytes(x.format.trailingSkipBytes.
            getOrElse(AnnotationDefaults defaultTrailingSkipBytes))

    val lengthKind = x.format.lengthKind.getOrElse(AnnotationDefaults defaultLengthKind)

    binaryProcessor setLengthKind(lengthKind)

    if (lengthKind==EndOfParent)
      binaryProcessor setEndOfParent (true)
    
    x.format.length match {
      case Some(l) => binaryProcessor setLength(l)
      case None => if (lengthKind==Explicit)
        throw new DFDLSchemaDefinitionException("Missing length property",schemaContext = x element)
    }

    x.format.typeName.map { binaryProcessor.setTypeName(_) }

    binaryProcessor
  }
  
  private def wrapDebugger(annotation:Annotation,processor:BasicProcessor):BasicProcessor = {
    if (debugger==null)
      processor
    else
      new DebuggingBasicProcessor(annotation,debugger,processor)
  }
  
  private def wrapDebugger(annotation:Annotation,processor:InternalProcessor):InternalProcessor = {
    if (debugger==null)
      processor
    else
      new DebuggingInternalProcessor(annotation,debugger,processor)
  }
  
  def setDebugger(debugger:DebuggingListener) = this.debugger = debugger
  
}
