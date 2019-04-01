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

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.util.LoggingDefaults
import org.apache.daffodil.util.LogLevel

object TestNamespaces {
  val testDir = "/org/apache/daffodil/section06/namespaces/"

  val runner = Runner(testDir, "namespaces.tdml", validateTDMLFile = true, validateDFDLSchemas = false)
  val runnerV = Runner(testDir, "namespaces.tdml", validateTDMLFile = true, validateDFDLSchemas = true)

  val runner2 = Runner(testDir, "multiFile.tdml", validateTDMLFile = false, validateDFDLSchemas = false)
  val runner3 = Runner(testDir, "includeImport.tdml")
  val runnerWithSchemaValidation = Runner(testDir, "multiFile.tdml", validateTDMLFile = true, validateDFDLSchemas = true)

  @AfterClass def shutDown {
    runner.reset
    runner2.reset
    runner3.reset
    runnerWithSchemaValidation.reset
  }
}
class TestNamespaces {

  import TestNamespaces._

  @Test def test_schemaNoGlobalElems_01() { runner.runOneTest("schemaNoGlobalElems_01") }
  @Test def test_schemaNoGlobalElems_02() { runner.runOneTest("schemaNoGlobalElems_02") }

  @Test def test_schemaSameDir_01() { runner.runOneTest("schemaSameDir_01") }
  @Test def test_schemaSameDir_02() { runner.runOneTest("schemaSameDir_02") }
  @Test def test_schemaSameDir_03() { runner.runOneTest("schemaSameDir_03") }

  @Test def test_schemaSameDirClasspath_01() { runner.runOneTest("schemaSameDirClasspath_01") }

  // See comments in related bug. JIRA-549
  // This test is looking for a specific file to be mentioned in an error message
  // which is the file with the content responsible for the error, not the file
  // of the object where the error was detected.

  @Test def test_combinations_02() {
    try {
      // Must turn off the Info logging messages, because those will have the filename in them
      // which would create a false positive in this test.
      LoggingDefaults.setLoggingLevel(LogLevel.Warning)
      runnerV.runOneTest("combinations_02")
    } finally {
      LoggingDefaults.setLoggingLevel(LogLevel.Info)
    }
  }

  @Test def test_errorLocations_01() {
    try {
      // Must turn off the Info logging messages, because those will have the filename in them
      // which would create a false positive in this test.
      LoggingDefaults.setLoggingLevel(LogLevel.Warning)
      runner.runOneTest("errorLocations_01")
    } finally {
      LoggingDefaults.setLoggingLevel(LogLevel.Info)
    }
  }

  @Test def test_defaultNamespaceInExpression() { runner.runOneTest("defaultNamespaceInExpression") }
  @Test def test_defaultNamespaceInExpression2() { runner.runOneTest("defaultNamespaceInExpression2") }

  @Test def test_namespaces_qnames() { runner.runOneTest("namespaces_qnames") }
  @Test def test_namespaces_qnames2() { runner.runOneTest("namespaces_qnames2") }
  @Test def test_namespaces_qnames3() { runner.runOneTest("namespaces_qnames3") }

  // DFDL-1663
  // @Test def test_namespaceLimitParse() { runner.runOneTest("namespaceLimitParse")  }
  // @Test def test_namespaceLimitUnparse() { runner.runOneTest("namespaceLimitUnparse")  }

  // DFDL-1204 - this test no longer works. New loader won't accept character U+00B7 as a character
  // in a prefix name.
  // @Test def test_namespaceSpecialChars() { runner.runOneTest("namespaceSpecialChars") }
  @Test def test_namespaceSpecialChars2() { runnerV.runOneTest("namespaceSpecialChars2") }
  @Test def test_namespaceRules1() { runner.runOneTest("namespaceRules1") }
  @Test def test_namespaceRules2() { runnerV.runOneTest("namespaceRules2") }

  @Test def testSimpleIncludeOfFormat() { runner2.runOneTest("simpleInclude") }
  @Test def testSimpleImportOfFormat() { runner2.runOneTest("simpleImport") }
  @Test def testIncludeNoNamespace() { runner2.runOneTest("includeNoNamespace") }
  @Test def testImportWithOverlappingNSPrefixes1() { runner2.runOneTest("importWithOverlappingNSPrefixes1") }

  @Test def test_tdml_schema_import() { runner.runOneTest("tdml_schema_import") }
  @Test def test_tdml_schema_include() { runner.runOneTest("tdml_schema_include") }
  @Test def test_multifile_choice_embed() { runner.runOneTest("multifile_choice_embed") }

  @Test def test_Lesson2_no_namespace() { runner.runOneTest("Lesson2_no_namespace") }
  @Test def test_Lesson2_include_schema() { runner.runOneTest("Lesson2_include_schema") }
  @Test def test_Lesson2_import_schema() { runner.runOneTest("Lesson2_import_schema") }

  @Test def test_multifile_cyclical() { runner.runOneTest("multifile_cyclical") }
  @Test def test_multifile_choice_01() { runner.runOneTest("multifile_choice_01") }
  @Test def test_multifile_choice_02() { runner.runOneTest("multifile_choice_02") }
  // DAFFODIL-553
  //  @Test def test_multifile_choice_02b() { runner.runOneTest("multifile_choice_02b") }
  @Test def test_multifile_choice_03() { runner.runOneTest("multifile_choice_03") }

  @Test def test_multifile_facets_01() { runner.runOneTest("multifile_facets_01") }
  @Test def test_multifile_facets_02() { runner.runOneTest("multifile_facets_02") }
  @Test def test_multifile_facets_03() { runner.runOneTest("multifile_facets_03") }
  @Test def test_multifile_facets_04() { runner.runOneTest("multifile_facets_04") }

  @Test def test_double_nesting_01() { runner.runOneTest("double_nesting_01") }

  @Test def test_scope_01() { runner.runOneTest("scope_01") }
  @Test def test_scope_02() { runner.runOneTest("scope_02") }

  @Test def test_long_chain_01() { runner.runOneTest("long_chain_01") }
  @Test def test_long_chain_02() { runner.runOneTest("long_chain_02") }
  @Test def test_long_chain_03() { runner.runOneTest("long_chain_03") }
  @Test def test_long_chain_04() { runner.runOneTest("long_chain_04") }
  @Test def test_long_chain_05() { runner.runOneTest("long_chain_05") }
  @Test def test_long_chain_06() { runner.runOneTest("long_chain_06") }
  @Test def test_long_chain_06b() { runner.runOneTest("long_chain_06b") }
  @Test def test_long_chain_07() { runner.runOneTest("long_chain_07") }

  @Test def test_no_namespace_01() { runner.runOneTest("no_namespace_01") }

  @Test def test_no_namespace_02() { runner.runOneTest("no_namespace_02") }
  @Test def test_no_namespace_03() { runner.runOneTest("no_namespace_03") }
  @Test def test_no_namespace_04() { runner.runOneTest("no_namespace_04") }

  @Test def test_namespace_conflict_01() { runner.runOneTest("namespace_conflict_01") }

  @Test def test_combinations_03() { runner.runOneTest("combinations_03") }
  @Test def test_combinations_04() { runner.runOneTest("combinations_04") }

  @Test def test_negative_import_01() { runner.runOneTest("negative_import_01") }

  @Test def test_multi_encoding_01() { runner.runOneTest("multi_encoding_01") }
  @Test def test_multi_encoding_02() { runner.runOneTest("multi_encoding_02") }
  @Test def test_multi_encoding_03() { runner.runOneTest("multi_encoding_03") }
  //  @Test def test_multi_encoding_04() { runner.runOneTest("multi_encoding_04") } //DFDL-716
  //  @Test def test_multi_encoding_05() { runner.runOneTest("multi_encoding_05") } //DFDL-715
  //  @Test def test_indexOutOfBounds_01() { runner.runOneTest("indexOutOfBounds_01") } // DAFFODIL-717

  // Preliminary tests for import format schemas
  @Test def test_import_format_01() { runner.runOneTest("import_format_01") }
  @Test def test_import_format_02() { runner.runOneTest("import_format_02") }

  @Test def test_multifile_facets_05() { runner.runOneTest("multifile_facets_05") }

  @Test def test_element_conflict_01() { runner.runOneTest("element_conflict_01") }
  @Test def test_element_conflict_02() { runner.runOneTest("element_conflict_02") }

  @Test def test_no_namespace_temp() { runnerV.runOneTest("no_namespace_temp") }

  @Test def test_lion_eater_ambiguity_01() { runner.runOneTest("lion_eater_ambiguity_01") }
  @Test def test_lion_eater_ambiguity_01b() { runnerV.runOneTest("lion_eater_ambiguity_01b") }
  @Test def test_lion_eater_ambiguity_02() { runner.runOneTest("lion_eater_ambiguity_02") }
  @Test def test_lion_eater_ambiguity_03() { runner.runOneTest("lion_eater_ambiguity_03") }
  @Test def test_lion_eater_ambiguity_04() { runner.runOneTest("lion_eater_ambiguity_04") }
  @Test def test_lion_eater_ambiguity_05() { runner.runOneTest("lion_eater_ambiguity_05") }

  @Test def test_namespace_ultra_uniqueness_01() { runner.runOneTest("namespace_ultra_uniqueness_01") }
  @Test def test_namespace_ultra_uniqueness_02() { runner.runOneTest("namespace_ultra_uniqueness_02") }
  @Test def test_namespace_ultra_uniqueness_03() { runner.runOneTest("namespace_ultra_uniqueness_03") }
  @Test def test_namespace_ultra_uniqueness_04() { runner.runOneTest("namespace_ultra_uniqueness_04") }

  @Test def test_primTypesPrefixes01() { runner.runOneTest("primTypesPrefixes01") }
  @Test def test_typeNameOverlap_01() { runner.runOneTest("typeNameOverlap_01") }
  @Test def test_typeNameOverlap_02() { runner.runOneTest("typeNameOverlap_02") }

  @Test def test_namespace_scope_01() { runner.runOneTest("namespace_scope_01") }
  @Test def test_namespace_scope_02() { runner.runOneTest("namespace_scope_02") }

  @Test def test_error_messages_01() { runnerV.runOneTest("error_messages_01") }

  @Test def test_ibm_format_compat_01() { runner.runOneTest("ibm_format_compat_01") }
  @Test def test_ibm_format_compat_02() { runner.runOneTest("ibm_format_compat_02") }
  @Test def test_ibm_format_compat_03() { runner.runOneTest("ibm_format_compat_03") }
  @Test def test_nonsense_namespace_01() { runner.runOneTest("nonsense_namespace_01") }
  @Test def test_nonsense_namespace_02() { runner.runOneTest("nonsense_namespace_02") }
  @Test def test_nonsense_namespace_03() { runner.runOneTest("nonsense_namespace_03") }
  @Test def test_nonsense_namespace_04() { runnerV.runOneTest("nonsense_namespace_04") }

  @Test def test_junkAnnotation01() { runner.runOneTest("junkAnnotation01") }

  @Test def test_include01() { runner3.runOneTest("include01") }
  @Test def test_include02() { runner3.runOneTest("include02") }
  @Test def test_toplevel_annotation_invalid_01() { runner.runOneTest("toplevel_annotation_invalid_01") }
  @Test def test_toplevel_annotation_invalid_02() { runner.runOneTest("toplevel_annotation_invalid_02") }

  @Test def test_incorrectAppinfoSource() { runner.runOneTest("incorrectAppinfoSource") }
}
