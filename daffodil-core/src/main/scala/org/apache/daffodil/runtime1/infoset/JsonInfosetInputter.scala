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

package org.apache.daffodil.runtime1.infoset

import java.lang.{ Boolean => JBoolean }
import java.util.Optional

import org.apache.daffodil.api
import org.apache.daffodil.api.infoset.Infoset.InfosetInputterEventType
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.runtime1.dpath.NodeInfo

import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonToken

object JsonInfosetInputter {
  lazy val jsonFactory = new JsonFactory()
}

class JsonInfosetInputter(input: java.io.InputStream) extends api.infoset.InfosetInputter {

  private lazy val jsp = {
    val j = JsonInfosetInputter.jsonFactory.createParser(input)
    val tok = getNextToken(j)
    if (tok != JsonToken.START_OBJECT) {
      throw new IllegalContentWhereEventExpected(
        "Expected json content beginning with '{' but got '" + j.getText + "'"
      )
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
    val tok =
      try {
        j.nextToken()
      } catch {
        case e: JsonParseException => throw new IllegalContentWhereEventExpected(e.getMessage)
      }
    tok
  }

  override def getEventType(): InfosetInputterEventType = {
    import InfosetInputterEventType._
    if (fakeElementEndEvent) {
      EndElement
    } else {
      jsp.getCurrentToken() match {
        case JsonToken.START_OBJECT => if (objectDepth == 1) StartDocument else StartElement
        case JsonToken.END_OBJECT => EndElement
        case JsonToken.VALUE_STRING | JsonToken.VALUE_NUMBER_INT |
            JsonToken.VALUE_NUMBER_FLOAT | JsonToken.VALUE_TRUE | JsonToken.VALUE_FALSE |
            JsonToken.VALUE_NULL => {
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

  override def getLocalName(): String = {
    val curName = jsp.getCurrentName()
    if (curName == null) {
      // this only happens if we are directly inside an array, so get the value
      // off the array stack
      arrayNameStack.top
    } else {
      curName
    }
  }

  override def getSupportsNamespaces: Boolean = false

  override def getNamespaceURI(): String = {
    null
  }

  override def getSimpleText(primType: NodeInfo.Kind): String = {
    if (!jsp.getCurrentToken().isScalarValue()) {
      throw new NonTextFoundInSimpleContentException(
        "Unexpected array or object '" + getLocalName() + "' on line " + jsp
          .getTokenLocation()
          .getLineNr()
      )
    } else if (jsp.getCurrentToken() == JsonToken.VALUE_NULL) {
      null
    } else {
      // this handles unescaping any escaped characters
      jsp.getText()
    }
  }

  override def isNilled(): Optional[JBoolean] = {
    if (jsp.getCurrentToken() == JsonToken.VALUE_NULL) {
      Some(java.lang.Boolean.valueOf(true))
    } else {
      None
    }
  }

  override def fini(): Unit = {
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
          case JsonToken.START_OBJECT =>
            objectDepth += 1; exitNow = true
          case JsonToken.END_OBJECT =>
            objectDepth -= 1; exitNow = true

          // start of a simple type or null
          case token if token.isScalarValue() => exitNow = true

          // skip field names, jackson makes these available via
          // getCurrentName(), except for array elements
          case JsonToken.FIELD_NAME =>

          case _ =>
            throw new IllegalContentWhereEventExpected(
              "Unexpected json token '" + jsp
                .getText() + "' on line " + jsp.getTokenLocation().getLineNr()
            )
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
