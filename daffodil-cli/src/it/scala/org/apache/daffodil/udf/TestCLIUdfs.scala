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

import java.nio.file.Path

import org.junit.Test

import org.apache.daffodil.CLI.Util._
import org.apache.daffodil.Main.ExitCode

class TestCLIUdfs {

  /**
   * Return a sequence of paths, made up of the one classpath containing all
   * compiled UDF .class files, and any additional classpaths needed for a
   * specific test. In most cases, the extra should just include the path to
   * the directory containing the META-INF dir needed for the test.
   */
  private def udfClasspath(extra: String*): Seq[Path] = {
    val classes = path("daffodil-udf/target/scala-2.12/test-classes/")
    val paths = extra.map(path(_))
    classes +: paths
  }

  /**
   * Tests the case when no User Defined Functions on classpath, and a schema makes
   * no User Defined Function calls, so they don't get loaded
   */
  @Test def test_noUdfsLoaded_regular_schema(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")

    runCLI(args"-v parse -s $schema -r fn_func") { cli =>
      cli.send("strng", inputDone = true)
      cli.expect("<fn_func>")
      cli.expect("<data>strng</data>")
      cli.expect("<value>Hello,strng</value>")
      cli.expect("</fn_func>")
    } (ExitCode.Success)
  }

  /**
   * Tests the case when no User Defined Function are loaded, but a schema makes a
   * User Defined Function call
   */
  @Test def test_noUdfsLoaded_udf_schema(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")

    runCLI(args"-v parse -s $schema -r user_func1") { cli =>
      cli.send("strng", inputDone = true)
      cli.expectErr("[error] Schema Definition Error: Unsupported function: jsudf:replace")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when no User Defined Function are loaded, due to
   * ServiceConfigurationError such as an invalid META-INF file referencing a class
   * missing from the classPath
   *
   * The schema makes a User Defined Function call
   */
  @Test def test_noUdfsLoaded_MissingClassInMetaInfFile(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/resources/org/badmetainf/nonexistentclass/")

    runCLI(args"-v parse -s $schema -r user_func1", classpath) { cli =>
      cli.send("strng", inputDone = true)
      cli.expectErr("[warn] User Defined Function Provider failed to load: org.apache.daffodil.udf.UserDefinedFunctionProvider:")
      cli.expectErr("Provider org.nonexistentclass.example.StringFunctions.StringFunctionsProvider not found")
      cli.expectErr("[error] Schema Definition Error: Unsupported function: jsudf:replace")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when no User Defined Function are loaded, due to absent META-INF
   * file, but a schema makes a User Defined Function call
   */
  @Test def test_noUdfsLoaded_MissingMetaInfFile(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath()

    runCLI(args"-v parse -s $schema -r user_func1", classpath) { cli =>
      cli.send("strng", inputDone = true)
      cli.expectErr("[error] Schema Definition Error: Unsupported function: jsudf:replace")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when a User Defined Function Provider returns null for its list of UDFs
   */
  @Test def test_UDFPClass_NoUdfClasses(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/java/org/badudfs/functionclasses1/StringFunctions/")

    runCLI(args"-v parse -s $schema -r user_func1", classpath) { cli =>
      cli.send("strng", inputDone = true)
      cli.expectErr("[warn] User Defined Function Provider ignored:" +
        " org.badudfs.functionclasses1.StringFunctions.StringFunctionsProvider." +
        " No User Defined Functions found.")
      cli.expectErr("[error] Schema Definition Error: Unsupported function: jsudf:replace")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when a User Defined Function Provider returns an empty list of UDFs
   */
  @Test def test_UDFPClass_emptyUdfClasses(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/java/org/badudfs/functionclasses2/StringFunctions/")

    runCLI(args"-v parse -s $schema -r user_func1", classpath) { cli =>
      cli.sendLine("strng", inputDone = true)
      cli.expectErr("[warn] User Defined Function Provider ignored:" +
        " org.badudfs.functionclasses2.StringFunctions.StringFunctionsProvider." +
        " No User Defined Functions found.")
      cli.expectErr("[error] Schema Definition Error: Unsupported function: jsudf:replace")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when a function class from the UDFP doesn't implement the UDF
   * interface
   */
  @Test def test_UDFClass_nonUDF(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/java/org/badudfs/nonUDF/StringFunctions/")

    runCLI(args"-v parse -s $schema -r user_func1", classpath) { cli =>
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.nonUDF.StringFunctions.FuncA." +
        " Doesn't implement org.apache.daffodil.udf.UserDefinedFunction")
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.nonUDF.StringFunctions.Replace." +
        " Doesn't implement org.apache.daffodil.udf.UserDefinedFunction")
      cli.expectErr("[error] Schema Definition Error: Unsupported function: jsudf:replace")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when a function class doesn't have annotations or have empty/invalid
   * annotstion fields
   */
  @Test def test_UDFClass_nonAnn(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/java/org/badudfs/annotations/StringFunctions/")

    runCLI(args"-v parse -s $schema -r user_func1", classpath) { cli =>
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.annotations.StringFunctions.FuncB." +
        " Missing org.apache.daffodil.udf.UserDefinedFunctionIdentification annotation")
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.annotations.StringFunctions.Compare." +
        " Annotation namespace field is empty or invalid.")
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.annotations.StringFunctions.Replace." +
        " Annotation name field is empty or invalid.")
      cli.expectErr("[error] Schema Definition Error: Unsupported function: jsudf:replace")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the cases when a function class:
   *   doesn't have an evaluate function
   *   has overloaded the evaluate function
   *   has unsupported param types
   *   has unsupported return type
   */
  @Test def test_UDFClass_noEvaluate(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/java/org/badudfs/evaluate/StringFunctions/")

    runCLI(args"-v parse -s $schema -r user_func1", classpath) { cli =>
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.evaluate.StringFunctions.Replace." +
        " Missing evaluate method: urn:example:com:ext:badudfs:stringfunctions:replace")
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.evaluate.StringFunctions.FuncA." +
        " Overloaded evaluate method: urn:example:com:ext:badudfs:stringfunctions:funcA")
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.evaluate.StringFunctions.FuncB." +
        " Unsupported return type: void")
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.evaluate.StringFunctions.FuncC." +
        " Unsupported parameter type(s): String[],int[]")
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.evaluate.StringFunctions.FuncD." +
        " Unsupported parameter type(s): String[]")
      cli.expectErr("[warn] User Defined Function ignored:" +
        " org.badudfs.evaluate.StringFunctions.FuncE." +
        " Unsupported return type: String[]")
      cli.expectErr("[error] Schema Definition Error: Unsupported function: jsudf:replace")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when a function class:
   *   throws custom error on evaluate
   */
  @Test def test_UDFClass_CustomExceptionOnEvaluate(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/scala/org/sbadudfs/udfexceptions/evaluating/StringFunctions/")

    runCLI(args"parse -s $schema -r user_func2", classpath) { cli =>
      cli.send("strng", inputDone = true)
      cli.expectErr("[error] User Defined Function 'ssudf:reverse' Error. Cause: org.sbadudfs.udfexceptions.evaluating.StringFunctions.Reverse$CustomException: UDF Error!")
      cli.expectErr("at org.sbadudfs.udfexceptions.evaluating.StringFunctions.Reverse.evaluate(StringFunctionsProvider.scala:56)")
    } (ExitCode.UserDefinedFunctionError)
  }

  /**
   * Tests the case when a function class:
   *   throws processing error on evaluate
   */
  @Test def test_UDFClass_ProcessingErrorOnEvaluate(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/scala/org/sbadudfs/udfexceptions/evaluating/StringFunctions/")

    runCLI(args"parse -s $schema -r user_func3", classpath) { cli =>
      cli.expectErr("[error] Schema Definition Error: User Defined Function 'ssudf:rev-words' Error: UDF PE!")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when a function class:
   *   throws an error while being loaded
   */
  @Test def test_UDFClass_exceptionOnLoad(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/scala/org/sbadudfs/udfexceptions2/StringFunctions/")

    runCLI(args"-v parse -s $schema -r user_func3", classpath) { cli =>
      cli.expectErr("[error] User Defined Function could not be initialized: {http://example.com/scala/udf}rev-words.")
      cli.expectErr("Cause: org.sbadudfs.udfexceptions2.StringFunctions.ReverseWords$CustomException: UDF Error!")
      cli.expectErr("at org.sbadudfs.udfexceptions2.StringFunctions.ReverseWords.<init>(StringFunctionsProvider.scala:65)")
    } (ExitCode.UserDefinedFunctionError)
  }

  /**
   * Tests the case when a provider class:
   *   throws an error while loading its UDFs
   */
  @Test def test_UDFPClass_exceptionOnLoadingUDFs(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/scala/org/sbadudfs/udfpexceptions/StringFunctions/")

    runCLI(args"-v parse -s $schema -r user_func3", classpath) { cli =>
      cli.expectErr("[warn] User Defined Function Provider ignored:")
      cli.expectErr("org.sbadudfs.udfpexceptions.StringFunctions.StringFunctionsProvider")
      cli.expectErr("Error loading User Defined Functions:")
      cli.expectErr("org.sbadudfs.udfpexceptions.StringFunctions.StringFunctionsProvider$CustomException")
      cli.expectErr("[error] Schema Definition Error: Unsupported function: ssudf:rev-words")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when a provider class:
   *   throws an error while being loaded
   */
  @Test def test_UDFPClass_exceptionOnLoad(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/scala/org/sbadudfs/udfpexceptions2/StringFunctions/")

    runCLI(args"-v parse -s $schema -r user_func3", classpath) { cli =>
      cli.expectErr("[warn] User Defined Function Provider failed to load: org.apache.daffodil.udf.UserDefinedFunctionProvider")
      cli.expectErr("Provider org.sbadudfs.udfpexceptions2.StringFunctions.StringFunctionsProvider could not be instantiated")
      cli.expectErr("[error] Schema Definition Error: Unsupported function: ssudf:rev-words")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when a provider class:
   *   incorrectly implements createUserDefinedFunction and returns the incorrect function
   *   class object
   */
  @Test def test_UDFPClass_incorrectUDFObject(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/scala/org/sbadudfs/functionclasses/StringFunctions/")

    runCLI(args"-v parse -s $schema -r user_func3", classpath) { cli =>
      cli.expectErr("[warn] User Defined Function class mismatch: {http://example.com/scala/udf}rev-words.")
      cli.expectErr("Expected: class org.sbadudfs.functionclasses.StringFunctions.ReverseWords")
      cli.expectErr("Actual: class org.sbadudfs.functionclasses.StringFunctions.Reverse")
      cli.expectErr("[error] Schema Definition Error: Unsupported function: ssudf:rev-words")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when a provider class:
   *   incorrectly implements createUserDefinedFunction that results in an exception
   */
  @Test def test_UDFPClass_incorrectUDFCreateImplementation(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/scala/org/sbadudfs/functionclasses/StringFunctions/")

    runCLI(args"-v parse -s $schema -r user_func2", classpath) { cli =>
      cli.expectErr("[error] User Defined Function could not be initialized: {http://example.com/scala/udf}reverse.")
      cli.expectErr("Cause: scala.MatchError: http://example.com/scala/udf:reverse (of class java.lang.String)")
      cli.expectErr("at org.sbadudfs.functionclasses.StringFunctions.StringFunctionsProvider.createUserDefinedFunction(StringFunctionsProvider.scala:34)")
    } (ExitCode.UserDefinedFunctionError)
  }

  /**
   * Tests the case when a UDF class:
   *    contains a non serializable member
   */
  @Test def test_UDFClass_serializability(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/scala/org/sbadudfs/functionclasses2/StringFunctions/")

    runCLI(args"-vv save-parser -s $schema -r user_func4", classpath) { cli =>
      cli.expectErr("[debug] User Defined Function loaded: org.sbadudfs.functionclasses2.StringFunctions.GetNonSerializableState => {http://example.com/scala/udf}get-nonserializable-state")
      cli.expectErr("[debug] User Defined Function loaded: org.sbadudfs.functionclasses2.StringFunctions.GetSerializableState => {http://example.com/scala/udf}get-serializable-state")
      cli.expectErr("[warn] User Defined Function is not serializable: org.sbadudfs.functionclasses2.StringFunctions.GetNonSerializableState.")
      cli.expectErr("Could not serialize member of class: org.sbadudfs.functionclasses2.StringFunctions.SomeNonSerializableClass")
      cli.expectErr("[error] Schema Definition Error: Unsupported function: ssudf:get-nonserializable-state")
    } (ExitCode.UnableToCreateProcessor)
  }

  /**
   * Tests the case when a UDF class:
   *    contains serializable member
   */
  @Test def test_UDFClass_serializability2(): Unit = {
    val schema = path("daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd")
    val classpath = udfClasspath("daffodil-udf/src/test/scala/org/sbadudfs/functionclasses2/StringFunctions/")

    withTempFile { parser =>
      runCLI(args"-vv save-parser -s $schema -r user_func5 $parser", classpath) { cli =>
        cli.expectErr("[debug] User Defined Function loaded: org.sbadudfs.functionclasses2.StringFunctions.GetNonSerializableState => {http://example.com/scala/udf}get-nonserializable-state")
        cli.expectErr("[debug] User Defined Function loaded: org.sbadudfs.functionclasses2.StringFunctions.GetSerializableState => {http://example.com/scala/udf}get-serializable-state")
      } (ExitCode.Success)

      runCLI(args"-v parse -P $parser", classpath) { cli =>
        cli.send("strng", inputDone = true)
        cli.expect("<user_func5>")
        cli.expect("<data>strng</data>")
        cli.expect("<value>Serializable State</value>")
        cli.expect("</user_func5>")
      } (ExitCode.Success)
    }
  }
}
