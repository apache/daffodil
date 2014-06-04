package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.collection.mutable.ArrayBuffer
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.api.DFDL

/**
 * Used to navigate from the Infoset back to schema components relevant
 * to a part of the Infoset.
 */
class SchemaComponentRegistry {

  private val contextMap: ArrayBuffer[ElementBase] = ArrayBuffer.empty
    
  def getComponentByID(uid: Int): Option[ElementBase] = {
    // TODO: why this try/catch??
    Assert.usage(uid >= 0)
    Assert.usage(uid < contextMap.length)
    val res = try {
      contextMap(uid)
    } catch {
      case u: UnsuppressableException => throw u
      case e: Exception => {
        Assert.impossibleCase() // Let's see if this happens. 
        // TODO: if the above never happens, eliminate the try/catch
      }
    }
    Some(res)
  }

  def getIDByName(name: String, ns: org.jdom2.Namespace) = {
    val targetNS = NS(ns)
    val index = contextMap.indexWhere(eb => eb.targetNamespace == targetNS && eb.name == name)
    Assert.invariant(index >= 0)
    index
  }

  def addComponent(sc: ElementBase): Int = {
    contextMap.append(sc)
    contextMap.length - 1
  }

  def getSchemas(): Option[Seq[String]] = {
    if (contextMap.size > 0) {
      val e = contextMap(0)
      val schemas = e.schemaDocument.schemaSet.schemas.map(s => s.fileName)
      Some(schemas)
    } else None
  }
}

object SchemaComponentRegistry {

  private case class TLSCR (var scr: SchemaComponentRegistry, var dataProc: DFDL.DataProcessor)
  
  private val tl = new ThreadLocal[TLSCR] {
    override def initialValue = {
      val s = new TLSCR(null, null)
      s
    }
  }
  
  def setup (scr: SchemaComponentRegistry) {
    tl.get.scr = scr
  }
  
  def setup (dataProc: DFDL.DataProcessor) {
    tl.get.dataProc = dataProc
  }
  
  def getSCR = {
    val scr = tl.get().scr
    Assert.usage(scr != null, "Must setup before get.")
    scr
  }
  
  def getDataProc = {
    val dataProc = tl.get().dataProc
    Assert.usage(dataProc != null, "Must setup before get.")
    dataProc
  }
}
