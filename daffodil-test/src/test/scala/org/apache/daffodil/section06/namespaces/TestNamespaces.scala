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

package org.apache.daffodil.section06.namespaces

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Ignore
import org.junit.Test

object TestNamespaces extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section06/namespaces/namespaces.tdml"
  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = true, validateDFDLSchemas = false)
}

object TestNamespacesValidate extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section06/namespaces/namespaces.tdml"
  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = true, validateDFDLSchemas = true)
}

object TestMultiFile extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section06/namespaces/multiFile.tdml"
  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = false, validateDFDLSchemas = false)
}

object TestIncludeImport extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section06/namespaces/includeImport.tdml"
}

class TestNamespaces extends TdmlTests {
  val tdmlSuite = TestNamespaces

  @Test def schemaNoGlobalElems_01 = test
  @Test def schemaNoGlobalElems_02 = test
  @Test def schemaNoGlobalElems_03 = test

  @Test def schemaSameDir_01 = test
  @Test def schemaSameDir_02 = test
  @Test def schemaSameDir_03 = test

  @Test def schemaSameDirClasspath_01 = test

  @Test def errorLocations_01 = test

  @Test def defaultNamespaceInExpression = test
  @Test def defaultNamespaceInExpression2 = test

  @Test def namespaces_qnames = test
  @Test def namespaces_qnames2 = test
  @Test def namespaces_qnames3 = test

  // DFDL-1663
  @Ignore @Test def namespaceLimitParse = test
  @Ignore @Test def namespaceLimitUnparse = test

  // DFDL-1204 - this test no longer works. New loader won't accept character U+00B7 as a character
  // in a prefix name.
  @Ignore @Test def namespaceSpecialChars = test

  @Test def namespaceRules1 = test

  @Test def tdml_schema_import = test
  @Test def tdml_schema_include = test
  @Test def multifile_choice_embed = test

  @Test def Lesson2_no_namespace = test
  @Test def Lesson2_include_schema = test
  @Test def Lesson2_import_schema = test

  @Test def multifile_cyclical = test
  @Test def multifile_choice_01 = test
  @Test def multifile_choice_02 = test
  // DAFFODIL-553
  @Ignore @Test def multifile_choice_02b = test
  @Test def multifile_choice_03 = test

  @Test def multifile_facets_01 = test
  @Test def multifile_facets_02 = test
  @Test def multifile_facets_03 = test
  @Test def multifile_facets_04 = test

  @Test def double_nesting_01 = test

  @Test def scope_01 = test
  @Test def scope_02 = test

  @Test def long_chain_01 = test
  @Test def long_chain_02 = test
  @Test def long_chain_03 = test
  @Test def long_chain_04 = test
  @Test def long_chain_05 = test
  @Test def long_chain_06 = test
  @Test def long_chain_06b = test
  @Test def long_chain_07 = test

  @Test def no_namespace_01 = test

  @Test def no_namespace_02 = test
  @Test def no_namespace_03 = test
  @Test def no_namespace_04 = test

  @Test def namespace_conflict_01 = test

  @Test def combinations_03 = test
  @Test def combinations_04 = test

  @Test def negative_import_01 = test

  @Test def multi_encoding_01 = test
  @Test def multi_encoding_02 = test
  @Test def multi_encoding_03 = test
  @Ignore @Test def multi_encoding_04 = test
  @Ignore @Test def multi_encoding_05 = test
  @Ignore @Test def indexOutOfBounds_01 = test

  // Preliminary tests for import format schemas
  @Test def import_format_01 = test
  @Test def import_format_02 = test

  @Test def multifile_facets_05 = test

  @Test def element_conflict_01 = test
  @Test def element_conflict_02 = test

  @Test def lion_eater_ambiguity_01 = test

  @Test def lion_eater_ambiguity_02 = test
  @Test def lion_eater_ambiguity_03 = test
  @Test def lion_eater_ambiguity_04 = test
  @Test def lion_eater_ambiguity_05 = test

  @Test def namespace_ultra_uniqueness_01 = test
  @Test def namespace_ultra_uniqueness_02 = test
  @Test def namespace_ultra_uniqueness_03 = test
  @Test def namespace_ultra_uniqueness_04 = test

  @Test def primTypesPrefixes01 = test
  @Test def typeNameOverlap_01 = test
  @Test def typeNameOverlap_02 = test

  @Test def namespace_scope_01 = test
  @Test def namespace_scope_02 = test

  @Test def ibm_format_compat_01 = test
  @Test def ibm_format_compat_02 = test
  @Test def ibm_format_compat_03 = test
  @Test def nonsense_namespace_01 = test
  @Test def nonsense_namespace_02 = test
  @Test def nonsense_namespace_03 = test

  @Test def junkAnnotation01 = test

  @Test def toplevel_annotation_invalid_01 = test
  @Test def toplevel_annotation_invalid_02 = test

  @Test def incorrectAppinfoSource = test

  @Test def noTargetNamespace_01 = test
  @Test def noTargetNamespace_02 = test
}

class TestNamespacesValidate extends TdmlTests {
  val tdmlSuite = TestNamespacesValidate

  // See comments in related bug. JIRA-549
  // This test is looking for a specific file to be mentioned in an error message
  // which is the file with the content responsible for the error, not the file
  // of the object where the error was detected.
  @Test def combinations_02 = test
  @Test def namespaceSpecialChars2 = test
  @Test def namespaceRules2 = test

  @Test def no_namespace_temp = test

  @Test def lion_eater_ambiguity_01b = test

  @Test def error_messages_01 = test

  @Test def nonsense_namespace_04 = test
}

class TestMultiFile extends TdmlTests {
  val tdmlSuite = TestMultiFile

  @Test def simpleInclude = test
  @Test def simpleImport = test
  @Test def includeNoNamespace = test
  @Test def importWithOverlappingNSPrefixes1 = test
  @Test def complexIncludesNamespaces_01 = test
  @Test def complexIncludesNamespaces_02 = test
}

class TestIncludeImport extends TdmlTests {
  val tdmlSuite = TestIncludeImport
  @Test def include01 = test
  @Test def include02 = test

  @Test def deprecatedSchemaLocation01 = test

  @Test def generalFormat01 = test
  @Test def generalFormat02 = test
  @Test def generalFormat03 = test
  @Test def generalFormat04 = test
}
