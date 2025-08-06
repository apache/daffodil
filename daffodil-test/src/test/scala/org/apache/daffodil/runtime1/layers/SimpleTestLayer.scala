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

package org.apache.daffodil.runtime1.layers

// different subpackage on purpose to test package private methods
import java.io.InputStream
import java.io.OutputStream

import org.apache.daffodil.api.layers.Layer

abstract class TLayer(n: String, ns: String) extends Layer(n, ns) {
  protected var intVar: Int = 0
  protected var stringVar: String = _
  override def wrapLayerInput(jis: InputStream): InputStream = jis
  override def wrapLayerOutput(jos: OutputStream): OutputStream = jos
}

/**
 * STL means "Simple Test Layer"
 */
final class STL_Ok1() extends TLayer("stlOk1", "urn:STL") {
  private[layers] def setLayerVariableParameters(intVar: Int, stringVar: String): Unit = {
    this.intVar = intVar
    this.stringVar = stringVar
  }
//  def getLayerVariableResult_intVar: Int = intVar + intVar
//  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_Ok2() extends TLayer("stlOk2", "urn:STL") {
  private[layers] def setLayerVariableParameters(): Unit = {
    this.intVar = 42
    this.stringVar = "forty two"
  }
  def getLayerVariableResult_intVar: Int = intVar + intVar
  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_Ok3() extends TLayer("stlOk3", "urn:STL") {
  private[layers] def setLayerVariableParameters(intVar: Int): Unit = {
    this.intVar = intVar
    this.stringVar = "forty two"
  }
  //  def getLayerVariableResult_intVar: Int = intVar + intVar
  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_Ok4() extends TLayer("stlOk4", "urn:STL") {
  private[layers] def setLayerVariableParameters(stringVar: String): Unit = {
    this.intVar = 42
    this.stringVar = stringVar
  }
  def getLayerVariableResult_intVar: Int = intVar + intVar
  //  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_BadTypeInLayerCode1() extends TLayer("stlBadTypeInLayerCode1", "urn:STL") {

  private var myOtherIntVar: String = null

  private[layers] def setLayerVariableParameters(intVar: String): Unit = { // type is String, should be Int
    this.myOtherIntVar = intVar
    this.stringVar = "forty two"
  }
  //  def getLayerVariableResult_intVar: Int = intVar + intVar
  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_BadTypeInLayerCode2() extends TLayer("stlBadTypeInLayerCode2", "urn:STL") {

  private var myOtherStringVar: Int = 0

  private[layers] def setLayerVariableParameters(intVar: Int): Unit = {
    this.intVar = intVar
    this.stringVar = "forty two"
  }
  //  def getLayerVariableResult_intVar: Int = intVar + intVar
  def getLayerVariableResult_stringVar: Int = intVar * 2 // type is Int, should be String
}

final class STL_NotInMETAINFServices() extends TLayer("stlBadNotInMETAINFServices", "urn:STL") {

  private[layers] def setLayerVariableParameters(intVar: Int): Unit = { // type is String, should be Int
    this.intVar = intVar
    this.stringVar = "forty two"
  }
  //  def getLayerVariableResult_intVar: Int = intVar + intVar
  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_BadMissingSetter() extends TLayer("stlBadMissingSetter", "urn:STL") {

//  private[layers] def setLayerVariableParameters(intVar: Int): Unit = {  // Whoops. Error is that the setter is missing
//    this.intVar = intVar
//    this.stringVar = "forty two"
//  }

  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_BadMissingSetterArg() extends TLayer("stlBadMissingSetterArg", "urn:STL") {

  private[layers] def setLayerVariableParameters()
    : Unit = { // Whoops. Error is that the setter arg intVar is missing
    this.intVar = 42
    this.stringVar = "forty two"
  }

  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_BadMissingGetter() extends TLayer("stlBadMissingGetter", "urn:STL") {

  private[layers] def setLayerVariableParameters(intVar: Int): Unit = {
    this.intVar = intVar
    this.stringVar = "forty two"
  }

  // Whoops. Error is that the getter isn't defined.
  // def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_BadMissingSetterVar() extends TLayer("stlBadMissingSetterVar", "urn:STL1") {

  private[layers] def setLayerVariableParameters(intVar: Int): Unit = { // Whoops. Error is that there is no var named intVar
    this.intVar = 42
    this.stringVar = "forty two"
  }

  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_BadMissingGetterVar() extends TLayer("stlBadMissingGetterVar", "urn:STL2") {

  private[layers] def setLayerVariableParameters(intVar: Int): Unit = {
    this.intVar = 42
    this.stringVar = "forty two"
  }

  def getLayerVariableResult_stringVar: String =
    stringVar + " " + stringVar // Whoops. Error is that there is no var named stringVar
}

final class STL_BadMissingDefaultConstrutor(intArg: Int)
  extends TLayer("stlBadMissingDefaultConstructor", "urn:STL") {

  private[layers] def setLayerVariableParameters(intVar: Int): Unit = {
    this.intVar = intVar + intArg
    this.stringVar = "forty two"
  }

  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}

final class STL_BadNotALayer() { // extends TLayer("stlBadMissingDefaultConstructor", "urn:STL") {

  var intVar: Int = 0
  var stringVar: String = _

  private[layers] def setLayerVariableParameters(intVar: Int): Unit = {
    this.intVar = intVar
    this.stringVar = "forty two"
  }

  def getLayerVariableResult_stringVar: String = stringVar + " " + stringVar
}
