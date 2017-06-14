/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.infoset

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.MStackOf
import edu.illinois.ncsa.daffodil.util.MaybeBoolean

import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonToken
import com.fasterxml.jackson.core.JsonParseException

object JsonInfosetInputter {
  lazy val jsonFactory = new JsonFactory()
}

class JsonInfosetInputter(reader: java.io.Reader)
  extends InfosetInputter {

  private lazy val jsp = {
    val j = JsonInfosetInputter.jsonFactory.createParser(reader)
    val tok = getNextToken(j)
    if (tok != JsonToken.START_OBJECT) {
      throw new IllegalContentWhereEventExpected("Expected json content beginning with '{' but got '" + j.getText + "'")
    }
    objectDepth += 1
    j
  }

  /**
   * Used to keep track of the name of arrays that we are in. The name of each
   * array we enter is pushed onto this stack and is used as the name for all
   * StartElement events immediately inside that array.
   */
  private val arrayNameStack = new MStackOf[String]()

  /**
   * The jackson library has no concept of an end event for values (which
   * represent simple types and nilled complex types). So if we see a json
   * value we use these flags to signifiy that the next event should become a
   * fake element end for this value.
   */
  private var fakeElementEndEvent = false
  private var nextEventShouldBeFakeEnd = false

  /**
   * Used to determine the depth of objects we've see so that we know when we
   * have finished seen that last object close
   */
  private var objectDepth = 0

  private def getNextToken(j: JsonParser) = {
    val tok = try {
      j.nextToken()
    } catch {
      case e: JsonParseException => throw new IllegalContentWhereEventExpected(e.getMessage)
    }
    tok
  }

  override def getEventType(): InfosetInputterEventType = {
    if (fakeElementEndEvent) {
      EndElement
    } else {
      jsp.getCurrentToken() match {
        case JsonToken.START_OBJECT => if (objectDepth == 1) StartDocument else StartElement
        case JsonToken.END_OBJECT => EndElement
        case JsonToken.VALUE_STRING | JsonToken.VALUE_NULL => {
          // we don't want to start faking element end yet, but signify that
          // after a call to next(), we will want to fake it
          nextEventShouldBeFakeEnd = true
          StartElement
        }
        case null => EndDocument
        case _ => Assert.impossible()
      }
    }
  }

  override def getLocalName: String = {
    val curName = jsp.getCurrentName()
    if (curName == null) {
      // this only happens if we are directly inside an array, so get the value
      // off the array stack
      arrayNameStack.top
    } else {
      curName
    }
  }

  override val supportsNamespaces = false

  override def getNamespaceURI(): String = {
    null
  }

  override def getSimpleText(): String = {
    if (jsp.getCurrentToken() == JsonToken.VALUE_NULL) {
      null
    } else {
      Assert.invariant(jsp.getCurrentToken() == JsonToken.VALUE_STRING)
      // this handles unescaping any escaped characters
      jsp.getText()
    }
  }

  override def isNilled(): MaybeBoolean = {
    if (jsp.getCurrentToken() == JsonToken.VALUE_NULL) {
      MaybeBoolean(true)
    } else {
      MaybeBoolean.Nope
    }
  }

  override def fini: Unit = {
    jsp.close()
  }

  override def hasNext(): Boolean = {
    // This json library does not support a hasNext() method. However, we know
    // that hasNext will always be true unless we are at the very final closing
    // brace. In next(), when we reach the very final closing brace, we call
    // getNextToken(). If there was extra stuff passed that brace,
    // getCurrentToken would not be null. Thus, if the current token is null,
    // that means we consumed the last brace and there is nothing next.
    jsp.getCurrentToken() != null
  }

  override def next(): Unit = {
    if (nextEventShouldBeFakeEnd) {
      nextEventShouldBeFakeEnd = false
      fakeElementEndEvent = true
    } else {
      fakeElementEndEvent = false

      var exitNow = false
      while (!exitNow) {
        getNextToken(jsp) match {
          // for arrays, just store the name of the array. When we see an
          // immediate child of an array (either START_OBJECT or VALUE_STRING),
          // then we will use the array name as its name
          case JsonToken.START_ARRAY => arrayNameStack.push(jsp.getCurrentName())
          case JsonToken.END_ARRAY => arrayNameStack.pop

          // start end of a complex type
          case JsonToken.START_OBJECT => objectDepth += 1; exitNow = true
          case JsonToken.END_OBJECT => objectDepth -= 1; exitNow = true

          // start of a simple type or null
          case JsonToken.VALUE_STRING | JsonToken.VALUE_NULL => exitNow = true

          // skip field names, jackson makes these available via
          // getCurrentName(), except for array elements
          case JsonToken.FIELD_NAME =>

          case _ =>
            throw new IllegalContentWhereEventExpected("Unexpected json token '" + jsp.getText() + "' on line " + jsp.getTokenLocation().getLineNr())
        }
      }

      if (objectDepth == 0) {
        // We consumed the wrapper object, the nextToken should be null, which we
        // use to signifity EndDocument and hasNext == false. Note that if
        // nextToken does not return null, Daffodil will later call hasNext() to
        // test if there are anymore events. It will return true and Daffodil
        // will throw an error. So we do not need to check if this is null here.
        getNextToken(jsp)
      }
    }
  }

}
