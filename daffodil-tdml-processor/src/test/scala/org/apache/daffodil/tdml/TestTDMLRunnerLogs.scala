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

package org.apache.daffodil.tdml

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.Implicits._
import org.junit.Assert.fail

import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.core.{Logger => Log4jLogger}
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.config.Configurator

object TestTDMLRunnerLogs {
  val runner = Runner("/test/tdml/", "testLogs.tdml")
  val tdmlAppender = TDMLAppender.createAppender

  if(LogManager.getRootLogger().asInstanceOf[Log4jLogger].getAppenders().get("org.apache.tdml")==null){
        val loggers = LogManager.getRootLogger().asInstanceOf[Log4jLogger]
        loggers.addAppender(tdmlAppender)
        tdmlAppender.start()
        Configurator.setLevel("org.apache.daffodil", Level.INFO)
    }

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestTDMLRunnerLogs {
  import TestTDMLRunnerLogs._

  @Test def test_logWhenExpectingSuccess() = { runner.runOneTest("logWhenExpectingSuccess", tdmlAppender) }
  @Test def test_logWhenExpectingError() = { runner.runOneTest("logWhenExpectingError", tdmlAppender) }
  @Test def test_logWhenExpectingWarning() = { runner.runOneTest("logWhenExpectingWarning", tdmlAppender) }
  @Test def test_unparserLogWhenExpectingSuccess() = { runner.runOneTest("unparserLogWhenExpectingSuccess", tdmlAppender) }
  @Test def test_unparserLogWhenExpectingError() = { runner.runOneTest("unparserLogWhenExpectingError", tdmlAppender) }
  @Test def test_unparserLogWhenExpectingWarning() = { runner.runOneTest("unparserLogWhenExpectingWarning", tdmlAppender) }

  /*
   * These tests insure that the TDML runner is actually testing the logs.
   * A TDML test could silently not even be checking the logs and pass with false positive.
   * These tests cause the TDML runner test to fail to find logs, and check
   * that we got the TDML runner to detect this.
   */

  @Test def testLogsCheckedParserExpectingSuccess() = {
    val testSuite =
      <tdml:testSuite suiteName="tdml logs" xmlns:tdml="http://www.ibm.com/xmlns/dfdl/testData" xmlns:dfdlx="http://www.ogf.org/dfdl/dfdl-1.0/extensions" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:ex="http://example.com" xmlns:fn="http://www.w3.org/2005/xpath-functions">
        <tdml:defineSchema name="causesLogs" elementFormDefault="unqualified">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="ex:GeneralFormat" lengthKind="implicit"/>
	      <xs:element name="output">
		    <xs:complexType>
			  <xs:sequence>
				<xs:sequence dfdl:hiddenGroupRef="hidden_message" />
				<xs:element name="message" type="xs:string" dfdl:inputValueCalc="{
					if (../Hidden_Value eq '0') then 'hello'
					else fn:error('Unrecognized value')}" dfdl:terminator="" />
			  </xs:sequence>
		    </xs:complexType>
	      </xs:element>
	      <xs:group name="hidden_message"> 
		    <xs:sequence>
		      <xs:element name="Hidden_Value" type="xs:string" dfdl:length="1" dfdl:lengthKind="explicit" dfdl:outputValueCalc="{
				if (../message eq 'hello') then '0'
				else fn:error('Unrecognized value')}"
		      />
		    </xs:sequence>
          </xs:group>
        </tdml:defineSchema>
        <tdml:parserTestCase name="warningWhenExpectingSuccess" root="output" model="causesLogs">
          <tdml:document><![CDATA[0]]></tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <ex:output>
                <message>hello</message>
              </ex:output>
            </tdml:dfdlInfoset>
          </tdml:infoset>
          <tdml:logs>
            <tdml:log>This will not be found</tdml:log>
          </tdml:logs>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    val e = intercept[Exception] {
      runner.runOneTest("warningWhenExpectingSuccess", tdmlAppender)
    }
    runner.reset
    val msg = e.getMessage()
    if (!msg.contains("This will not be found")) {
      fail("TDML Logs were not checked")
    }
  }
 
}
