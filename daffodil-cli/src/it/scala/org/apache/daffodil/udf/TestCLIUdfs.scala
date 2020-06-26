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

import org.junit.Test
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.anyOf
import net.sf.expectit.matcher.Matchers.allOf
import net.sf.expectit.matcher.Matchers.eof
import java.nio.file.FileSystems
import java.nio.file.Files
import scala.collection.JavaConverters._

class TestCLIUdfs {

  lazy val testUdfsPaths = {
    val dir = FileSystems.getDefault.getPath(Util.daffodilPath("daffodil-udf/target"))
    val allClassFiles = Files.walk(dir).iterator.asScala
      .filter { f => Files.isRegularFile(f) && f.toString.endsWith("class") }
    val commonPrefix = allClassFiles
      .flatMap {
        fp =>
          val filePath = fp.toString
          val indexOfOrg = filePath.indexOfSlice("org")
          if (indexOfOrg >= 0) {
            // use index - 1 to remove trailing slash, which can cause
            // accidental escaping on windows
            val prefix = filePath.splitAt(indexOfOrg - 1)._1
            Some(prefix)
          } else None
      }.toList.distinct
    commonPrefix
  }

  /**
   * Tests the case when no User Defined Functions on classpath, and a schema makes
   * no User Defined Function calls, so they don't get loaded
   */
  @Test def test_noUdfsLoaded_regular_schema(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r fn_func", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(
        allOf(
          contains("<fn_func>"),
          contains("<data>strng</data>"),
          contains("<value>Hello,strng</value>"),
          contains("</fn_func>")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when no User Defined Function are loaded, but a schema makes a
   * User Defined Function call
   */
  @Test def test_noUdfsLoaded_udf_schema(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val shell = Util.startIncludeErrors("")

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func1", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[info] No User Defined Functions loaded."),
          contains("[error] Schema Definition Error: Unsupported function: jsudf:replace")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when no User Defined Function are loaded, due to
   * ServiceConfigurationError such as an invalid META-INF file referencing a class
   * missing from the classPath
   *
   * The schema makes a User Defined Function call
   */
  @Test def test_noUdfsLoaded_MissingClassInMetaInfFile(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaForNonExistentClass = "daffodil-udf/src/test/resources/org/badmetainf/nonexistentclass/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaForNonExistentClass))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func1", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[error] Error while loading User Defined Function Providers:" +
            " org.apache.daffodil.udf.UserDefinedFunctionProvider:" +
            " Provider org.nonexistentclass.example.StringFunctions.StringFunctionsProvider not found"),
          contains("[info] No User Defined Functions loaded."),
          contains("[error] Schema Definition Error: Unsupported function: jsudf:replace")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when no User Defined Function are loaded, due to absent META-INF
   * file, but a schema makes a User Defined Function call
   */
  @Test def test_noUdfsLoaded_MissingMetaInfFile(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val dafClassPath =
      testUdfsPaths.mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func1", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[info] No User Defined Functions loaded."),
          contains("[error] Schema Definition Error: Unsupported function: jsudf:replace")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a User Defined Function Provider returns null for its list of UDFs
   */
  @Test def test_UDFPClass_NoUdfClasses(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/java/org/badudfs/functionclasses1/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func1", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[warning] User Defined Function Provider ignored:" +
            " org.badudfs.functionclasses1.StringFunctions.StringFunctionsProvider." +
            " No User Defined Functions found."),
          contains("[info] No User Defined Functions loaded."),
          contains("[error] Schema Definition Error: Unsupported function: jsudf:replace")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a User Defined Function Provider returns an empty list of UDFs
   */
  @Test def test_UDFPClass_emptyUdfClasses(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/java/org/badudfs/functionclasses2/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func1", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[warning] User Defined Function Provider ignored:" +
            " org.badudfs.functionclasses2.StringFunctions.StringFunctionsProvider." +
            " No User Defined Functions found."),
          contains("[info] No User Defined Functions loaded."),
          contains("[error] Schema Definition Error: Unsupported function: jsudf:replace")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a function class from the UDFP doesn't implement the UDF
   * interface
   */
  @Test def test_UDFClass_nonUDF(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/java/org/badudfs/nonUDF/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func1", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[warning] User Defined Function ignored:" +
            " org.badudfs.nonUDF.StringFunctions.FuncA." +
            " Doesn't implement org.apache.daffodil.udf.UserDefinedFunction"),
          contains("[warning] User Defined Function ignored:" +
            " org.badudfs.nonUDF.StringFunctions.Replace." +
            " Doesn't implement org.apache.daffodil.udf.UserDefinedFunction"),
          contains("[info] No User Defined Functions loaded."),
          contains("[error] Schema Definition Error: Unsupported function: jsudf:replace")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a function class doesn't have annotations or have empty/invalid
   * annotstion fields
   */
  @Test def test_UDFClass_nonAnn(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/java/org/badudfs/annotations/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func1", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[warning] User Defined Function ignored:" +
            " org.badudfs.annotations.StringFunctions.FuncB." +
            " Missing org.apache.daffodil.udf.UserDefinedFunctionIdentification annotation"),
          anyOf(
            contains("[warning] User Defined Function ignored:" +
              " org.badudfs.annotations.StringFunctions.Compare." +
              " Annotation namespace field is empty or invalid."),
            contains("[warning] User Defined Function ignored:" +
              " org.badudfs.annotations.StringFunctions.Compare." +
              " Annotation name field is empty or invalid.")),
          contains("[warning] User Defined Function ignored:" +
            " org.badudfs.annotations.StringFunctions.Replace." +
            " Annotation name field is empty or invalid."),
          contains("[info] No User Defined Functions loaded."),
          contains("[error] Schema Definition Error: Unsupported function: jsudf:replace")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the cases when a function class:
   *   doesn't have an evaluate function
   *   has overloaded the evaluate function
   *   has unsupported param types
   *   has unsupported return type
   */
  @Test def test_UDFClass_noEvaluate(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/java/org/badudfs/evaluate/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func1", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[warning] User Defined Function ignored:" +
            " org.badudfs.evaluate.StringFunctions.FuncA." +
            " Overloaded evaluate method: urn:example:com:ext:badudfs:stringfunctions:funcA"),
          contains("[warning] User Defined Function ignored:" +
            " org.badudfs.evaluate.StringFunctions.Replace." +
            " Missing evaluate method: urn:example:com:ext:badudfs:stringfunctions:replace"),
          contains("[warning] User Defined Function ignored:" +
            " org.badudfs.evaluate.StringFunctions.FuncB." +
            " Unsupported return type: void"),
          contains("[warning] User Defined Function ignored:" +
            " org.badudfs.evaluate.StringFunctions.FuncC." +
            " Unsupported parameter type(s): String[],int[]"),
          contains("[warning] User Defined Function ignored:" +
            " org.badudfs.evaluate.StringFunctions.FuncD." +
            " Unsupported parameter type(s): String[]"),
          contains("[warning] User Defined Function ignored:" +
            " org.badudfs.evaluate.StringFunctions.FuncE." +
            " Unsupported return type: String[]"),
          contains("[info] No User Defined Functions loaded."),
          contains("[error] Schema Definition Error: Unsupported function: jsudf:replace")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a function class:
   *   throws custom error on evaluate
   */
  @Test def test_UDFClass_CustomExceptionOnEvaluate(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/scala/org/sbadudfs/udfexceptions/evaluating/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s parse -s %s -r user_func2", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[error] User Defined Function 'ssudf:reverse' Error: UDF Error!"),
          contains("org.apache.daffodil.udf.UserDefinedFunctionFatalErrorException: "),
          contains("at org.sbadudfs.udfexceptions.evaluating.StringFunctions.Reverse.evaluate")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a function class:
   *   throws processing error on evaluate
   */
  @Test def test_UDFClass_ProcessingErrorOnEvaluate(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/scala/org/sbadudfs/udfexceptions/evaluating/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s parse -s %s -r user_func3", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(1, contains("[error] Schema Definition Error: User Defined Function 'ssudf:rev-words' Error: UDF PE!"))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a function class:
   *   throws an error while being loaded
   */
  @Test def test_UDFClass_exceptionOnLoad(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/scala/org/sbadudfs/udfexceptions2/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func3", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[error] Error initializing User Defined Function:"),
          contains("http://example.com/scala/udf:rev-words."),
          contains("Error thrown: org.sbadudfs.udfexceptions2.StringFunctions.ReverseWords$CustomException: UDF Error!"),
          contains("[error] User Defined Function 'http://example.com/scala/udf:rev-words' Error: UDF Error!"),
          contains("org.apache.daffodil.udf.UserDefinedFunctionFatalErrorException:"),
          contains("at org.sbadudfs.udfexceptions2.StringFunctions.ReverseWords")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a provider class:
   *   throws an error while loading its UDFs
   */
  @Test def test_UDFPClass_exceptionOnLoadingUDFs(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/scala/org/sbadudfs/udfpexceptions/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func3", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[warning] User Defined Function Provider ignored:"),
          contains("org.sbadudfs.udfpexceptions.StringFunctions.StringFunctionsProvider"),
          contains("Error loading User Defined Functions:"),
          contains("org.sbadudfs.udfpexceptions.StringFunctions.StringFunctionsProvider$CustomException"),
          contains("[error] Schema Definition Error: Unsupported function: ssudf:rev-words")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a provider class:
   *   throws an error while being loaded
   */
  @Test def test_UDFPClass_exceptionOnLoad(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/scala/org/sbadudfs/udfpexceptions2/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func3", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[error] Error while loading User Defined Function Providers:" +
            " org.apache.daffodil.udf.UserDefinedFunctionProvider:" +
            " Provider org.sbadudfs.udfpexceptions2.StringFunctions.StringFunctionsProvider could not be instantiated"),
          contains("[info] No User Defined Functions loaded."),
          contains("[error] Schema Definition Error: Unsupported function: ssudf:rev-words")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a provider class:
   *   incorrectly implements createUserDefinedFunction and returns the incorrect function
   *   class object
   */
  @Test def test_UDFPClass_incorrectUDFObject(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/scala/org/sbadudfs/functionclasses/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func3", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[error] User Defined Function Class Mismatch: http://example.com/scala/udf:rev-words." +
            " Expected: class org.sbadudfs.functionclasses.StringFunctions.ReverseWords" +
            " Actual: class org.sbadudfs.functionclasses.StringFunctions.Reverse"),
          contains("[error] Schema Definition Error: Unsupported function: ssudf:rev-words")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a provider class:
   *   incorrectly implements createUserDefinedFunction that results in an exception
   */
  @Test def test_UDFPClass_incorrectUDFCreateImplementation(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/scala/org/sbadudfs/functionclasses/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format(Util.echoN("strng") + "| %s -v parse -s %s -r user_func2", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[error] Error initializing User Defined Function:"),
          contains("http://example.com/scala/udf:reverse."),
          contains("Error thrown: scala.MatchError:"),
          contains("org.apache.daffodil.udf.UserDefinedFunctionFatalErrorException:"),
          contains("at org.sbadudfs.functionclasses.StringFunctions.StringFunctionsProvider.createUserDefinedFunction")))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a UDF class:
   *    contains a non serializable member
   */
  @Test def test_UDFClass_serializability(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val metaInfForSomeUdfA = "daffodil-udf/src/test/scala/org/sbadudfs/functionclasses2/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmd = String.format("%s -v save-parser -s %s -r user_func4", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(
        1,
        allOf(
          contains("[error] Error serializing initialized User Defined Function: org.sbadudfs.functionclasses2.StringFunctions.GetNonSerializableState"),
          contains("Could not serialize member of class: org.sbadudfs.functionclasses2.StringFunctions.SomeNonSerializableClass"),
          contains("[error] Schema Definition Error: Unsupported function: ssudf:get-nonserializable-state")))
      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /**
   * Tests the case when a UDF class:
   *    contains serializable member
   */
  @Test def test_UDFClass_serializability2(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val (testSchemaFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile)) else (schemaFile)

    val savedParserFile = java.io.File.createTempFile("testParser_", ".tmp")
    savedParserFile.deleteOnExit
    val metaInfForSomeUdfA = "daffodil-udf/src/test/scala/org/sbadudfs/functionclasses2/StringFunctions/"

    val dafClassPath =
      (testUdfsPaths :+ Util.daffodilPath(metaInfForSomeUdfA))
        .mkString(java.io.File.pathSeparator)

    val shell = Util.startIncludeErrors("", envp = Map("DAFFODIL_CLASSPATH" -> dafClassPath))

    try {
      val cmds = Array(
        String.format("%s -v save-parser -s %s -r user_func5 %s", Util.binPath, testSchemaFile, savedParserFile.getAbsolutePath),
        String.format(Util.echoN("strng") + "| %s -v parse -P %s", Util.binPath, savedParserFile.getAbsolutePath))
      val cmd = Util.makeMultipleCmds(cmds)
      shell.sendLine(cmd)
      shell.expectIn(
        0,
        allOf(
          contains("<user_func5>"),
          contains("<data>strng</data>"),
          contains("<value>Serializable State</value>"),
          contains("</user_func5>")))
      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
      savedParserFile.delete
    }
  }
}
