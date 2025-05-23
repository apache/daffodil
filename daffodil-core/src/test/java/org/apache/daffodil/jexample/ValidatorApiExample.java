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

package org.apache.daffodil.jexample;

import org.apache.daffodil.api.Daffodil;
import org.apache.daffodil.api.Diagnostic;
import org.apache.daffodil.api.ParseResult;
import org.apache.daffodil.api.DataProcessor;
import org.apache.daffodil.api.ProcessorFactory;
import org.apache.daffodil.api.infoset.Infoset;
import org.apache.daffodil.api.infoset.InfosetOutputter;
import org.apache.daffodil.api.infoset.JDOMInfosetOutputter;
import org.apache.daffodil.api.InputSourceDataInputStream;
import org.apache.daffodil.api.validation.Validator;
import org.apache.daffodil.api.validation.ValidatorInitializationException;
import org.apache.daffodil.api.validation.ValidatorNotRegisteredException;
import org.apache.daffodil.api.validation.Validators;
import org.apache.daffodil.jexample.validation.FailingValidator;
import org.apache.daffodil.jexample.validation.PassingValidator;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Properties;

import static org.junit.Assert.*;

public class ValidatorApiExample {
  private java.io.File getResource(String resPath) {
    try {
      return new java.io.File(this.getClass().getResource(resPath).toURI());
    } catch (Exception e) {
      return null;
    }
  }

  @Test
  public void testPassing() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema5.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/").withValidator(new PassingValidator());

    java.io.File file = getResource("/test/api/myData5.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      InfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);
      assertFalse(res.isValidationError());
    }
  }

  @Test
  public void testFailing() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    DataProcessor dp = pf.onPath("/").withValidator(new FailingValidator());

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      InfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);

      assertTrue(res.isValidationError());
    }
  }

  @Test // Verifies that Daffodil-2456 is a false report. Not a bug.
  public void testInvalidNoInfoset() throws IOException, ClassNotFoundException, ValidatorNotRegisteredException, ValidatorInitializationException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/alwaysInvalid.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);
    for (Diagnostic d : pf.getDiagnostics()) {
      System.err.println(d.getMessage());
    }
    DataProcessor dp1 = pf.onPath("/");
    Properties props = new Properties();
    ByteArrayInputStream bais = new ByteArrayInputStream(
        String.format("%s=%s", Validator.rootSchemaKey, schemaFile.toURI()).getBytes());
    props.load(bais);
    DataProcessor dp = dp1.withValidator(Validators.getInstance().get("xerces").make(props));

    java.io.InputStream fis = new ByteArrayInputStream("0".getBytes());
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);

      assertTrue(res.isValidationError());

      for (Diagnostic d : res.getDiagnostics()) {
        // doublecheck - all the errors are validation errors.
        // System.err.println(d.getMessage());
        assertTrue(d.getMessage().contains("Validation Error"));
      }
      assertTrue(res.isError());
      // XMLOutputter xout = new XMLOutputter();
      // xout.output(outputter.getResult(), System.out);
      // Now get the well formed, though invalid result.
      assertNotNull(outputter.getResult()); // Daffodil-2456 said this fails. It doesn't. Not reproducible.
    }
  }
}
