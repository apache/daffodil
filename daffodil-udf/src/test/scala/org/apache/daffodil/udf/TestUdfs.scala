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
package org.apache.daffodil.udf

import org.junit.AfterClass
import org.junit.Test
import junit.framework.Assert._
import java.net.URLClassLoader
import org.apache.daffodil.util.Misc
import org.apache.daffodil.udf
import java.net.URL
import collection.JavaConverters._

class TestUdfs {
  @Test def test_noUdfsLoaded() {
    try {
      lazy val addURL = classOf[URLClassLoader].getDeclaredMethod("addURL", classOf[URL]);
      print(Misc.classPath)
      val ucl: URLClassLoader = new URLClassLoader(Array(new URL("file:/to/nowhere/")))
      /*
            addURL.setAccessible(true)
      addURL.invoke(ucl, url)
      val urlsOfUcl = ucl.getURLs
      val uclAsCL = ucl.asInstanceOf[ClassLoader]
      val rsrcOfUCL = uclAsCL.getResources("META-INF/services/org.apache.daffodil.udf.UDFunctionProvider")*/
      val uclAsCL = ucl.asInstanceOf[ClassLoader]
      val emptyCL = ucl.getParent.getParent
      val udfs = new UDFunctionService(emptyCL)
      val errList = udfs.getErrors
      val warningsList = udfs.getWarnings
      assertEquals(1, errList.size())
      assertEquals(0, warningsList.size())
      assertTrue(errList.get(0).contains("No user defined functions found."))
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  @Test def test_Warnings() {
    try {
      val udfs = new UDFunctionService(null)
      val errList = udfs.getErrors
      val warningsList = udfs.getWarnings.asScala
      assertEquals(0, errList.size())

      // tests for missing or empty functionClasses
      assertTrue(warningsList.exists(_.contains(
        "Provider ignored: [org.badudfs.functionclasses1.StringFunctions.StringFunctionsProvider]." +
          "\nNo Function Classes found.")))

      // tests for missing annotations
      assertTrue(warningsList.exists(_.contains(
        "Provider ignored: [org.badudfs.annotations.StringFunctions.StringFunctionsProvider]." +
          "\nAnnotations missing for FunctionClass(es):")))

      // tests for empty annotations: missing both
      assertTrue(warningsList.exists(_.contains(
        "FunctionClass ignored: [org.badudfs.annotations.StringFunctions.Compare]. " +
          "From Provider [org.badudfs.annotations.StringFunctions.StringFunctionsProvider]." +
          "\nAnnotation namespace field is empty or invalid." +
          "\nAnnotation name field is empty or invalid.")))

      // tests for empty annotations; missing name
      assertTrue(warningsList.exists(_.contains(
        "FunctionClass ignored: [org.badudfs.annotations.StringFunctions.Compare]. " +
          "From Provider [org.badudfs.annotations.StringFunctions.StringFunctionsProvider]." +
          "\nAnnotation name field is empty or invalid.")))

      // tests non serializable function classes
      assertTrue(warningsList.exists(_.contains(
        "Provider ignored: [org.badudfs.nonserializable.StringFunctions.StringFunctionsProvider]. " +
          "\nFunctionClass(es) must implement java.io.Serializable:" +
          "\n[org.badudfs.nonserializable.StringFunctions.FuncA]")))

    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}