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

import org.apache.daffodil.runtime1.layers.api.Layer

final class TestLayer1()
  extends Layer(
    "testLayer1",
    "urn:org.apache.daffodil.layers.xsd.TestLayer1",
  ) {

  private var b1: Byte = 0
  private var ub1: Short = 0
  private var s1: Short = 0
  private var us1: Int = 0
  private var i1: Int = 0
  private var ui1: Long = 0
  private var l1: Long = 0
  private var ul1: java.math.BigInteger = null
  private var integer1: java.math.BigInteger = null
  private var nni1: java.math.BigInteger = null
  private var dec1: java.math.BigDecimal = null
  private var f1: Float = 0.0.toFloat
  private var d1: Double = 0.0
  private var string1: String = null
  private var hex1: Array[Byte] = null
  private var date1: com.ibm.icu.util.Calendar = null
  private var time1: com.ibm.icu.util.Calendar = null
  private var dt1: com.ibm.icu.util.Calendar = null

  private[layers] def setLayerVariableParameters(
    b1: Byte,
    ub1: Short,
    s1: Short,
    us1: Int,
    i1: Int,
    ui1: Long,
    l1: Long,
    ul1: java.math.BigInteger,
    integer1: java.math.BigInteger,
    nni1: java.math.BigInteger,
    dec1: java.math.BigDecimal,
    f1: Float,
    d1: Double,
    string1: String,
    hex1: Array[Byte],
//    date1: com.ibm.icu.util.Calendar,
//    time1: com.ibm.icu.util.Calendar,
//    dt1: com.ibm.icu.util.Calendar,
  ): Unit = {
    this.b1 = b1
    this.ub1 = ub1
    this.s1 = s1
    this.us1 = us1
    this.i1 = i1
    this.ui1 = ui1
    this.l1 = l1
    this.ul1 = ul1
    this.integer1 = integer1
    this.nni1 = nni1
    this.dec1 = dec1
    this.f1 = f1
    this.d1 = d1
    this.string1 = string1
    this.hex1 = hex1
//    this.date1 = date1
//    this.time1 = time1
//    this.dt1 = dt1
  }

  def getLayerVariableResult_b2(): Byte = b1
  def getLayerVariableResult_ub2(): Short = ub1
  def getLayerVariableResult_s2(): Short = s1
  def getLayerVariableResult_us2(): Int = us1
  def getLayerVariableResult_i2(): Int = i1
  def getLayerVariableResult_ui2(): Long = ui1
  def getLayerVariableResult_l2(): Long = l1
  def getLayerVariableResult_ul2(): java.math.BigInteger = ul1
  def getLayerVariableResult_integer2(): java.math.BigInteger = integer1
  def getLayerVariableResult_nni2(): java.math.BigInteger = nni1
  def getLayerVariableResult_dec2(): java.math.BigDecimal = dec1
  def getLayerVariableResult_f2(): Float = f1
  def getLayerVariableResult_d2(): Double = d1
  def getLayerVariableResult_string2(): String = string1
  def getLayerVariableResult_hex2(): Array[Byte] = hex1
//  def getLayerVariableResult_date2(): com.ibm.icu.util.Calendar = date1
//  def getLayerVariableResult_time2(): com.ibm.icu.util.Calendar = time1
//  def getLayerVariableResult_dt2(): com.ibm.icu.util.Calendar = dt1

  override def wrapLayerInput(jis: InputStream): InputStream = jis

  override def wrapLayerOutput(jos: OutputStream): OutputStream = jos
}
