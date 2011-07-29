package daffodil.exceptions

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

import org.jdom.Parent
import org.jdom.Document
import org.jdom.Element

import daffodil.xml.XMLUtil
import daffodil.debugger.DebugUtil
import java.io.ByteArrayOutputStream

/**
 * An exception indicating a problem related directly with the parsing of a document or DFDL schema
 *
 * @param description a message describing the problem
 * @param cause exception or error that caused this exception
 * @param schemaContext the context in the DFDL schema when the problem occured
 * @param documentContext the context in the document being built when the problem occured
 * @param position the position (in bytes) in the input stream being parsed
 *
 * @author Alejandro Rodriguez
 * @version 1 
 */
class DaffodilException(description:String,cause:Throwable=null,var schemaContext:Parent=null,
                        var documentContext:Parent=null,var position:Option[Long]=None)
	extends RuntimeException(description,cause)  {

  private val documentRoot = if (documentContext != null) XMLUtil.getRoot(documentContext) else null

  override def getMessage() =         {
    val sb = new StringBuffer
    sb append(description)
    if (schemaContext!=null){
      sb append("\n==========\n")
      sb append("At this point in the schema:")
      sb append("\n")
      sb append(
        schemaContext match {
          case e:Document => description + "Root";
          case e:Element => makeMessage(description,e)
          case _ => "Unknow"
        })
    }
    position match {
      case Some(x) =>
        sb append("\n==========\n")
        sb append("At byte number "+x+" of input\n")
      case None | null =>
    }
    if (documentRoot!=null && DebugUtil.verbose){
      sb append("\n==========\n")
      sb append("Partial generated output:")
      val byteArray = new ByteArrayOutputStream()
      try{
        XMLUtil serialize(byteArray,documentRoot)
      }catch{
        case _ => sb append("Partial output not available")
      }
      sb append (new String(byteArray toByteArray))
    }else if (documentRoot != null){
      sb append("Run in verbose mode to see partial output.")
    }

//    if (documentContext!=null){
//      sb append("\n==========\n")
//      sb append("At this point in the output document:")
//      sb append("\n")
//      sb append(
//        documentContext match {
//          case e:Document => description + "Root";
//          case e:Element => makeMessage(description,e)
//          case _ => "Unknow"
//        })
//    }
    sb toString
  }

  private def makeMessage(message:String,element:Element) =
    XMLUtil.printContext(element,"^^^^",80)
}
