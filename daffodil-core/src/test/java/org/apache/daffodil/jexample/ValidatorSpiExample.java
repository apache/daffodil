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

import org.apache.daffodil.api.infoset.Infoset;
import org.apache.daffodil.api.validation.ValidatorFactory;
import org.apache.daffodil.api.validation.Validators;
import org.apache.daffodil.jexample.validation.FailingValidator;
import org.apache.daffodil.jexample.validation.PassingValidator;
import org.apache.daffodil.api.Daffodil;
import org.apache.daffodil.api.DataProcessor;
import org.apache.daffodil.api.ParseResult;
import org.apache.daffodil.api.ProcessorFactory;
import org.apache.daffodil.api.infoset.JDOMInfosetOutputter;
import org.apache.daffodil.api.InputSourceDataInputStream;
import org.junit.Test;

import java.util.Properties;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class ValidatorSpiExample {
  private final Validators validators = Validators.getInstance();

  private java.io.File getResource(String resPath) {
    try {
      return new java.io.File(this.getClass().getResource(resPath).toURI());
    } catch (Exception e) {
      return null;
    }
  }

  @Test
  public void testPassing() throws Exception {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema5.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);

    assertTrue(validators.isRegistered(PassingValidator.name));
    ValidatorFactory vf = validators.get(PassingValidator.name);
    DataProcessor dp = pf.onPath("/").withValidator(vf.make(new Properties()));

    java.io.File file = getResource("/test/api/myData5.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);

      assertFalse(res.isValidationError());
    }
  }

  @Test
  public void testFailing() throws Exception {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();
    java.io.File schemaFile = getResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileFile(schemaFile);

    assertTrue(validators.isRegistered(FailingValidator.name));
    ValidatorFactory vf = validators.get(FailingValidator.name);
    DataProcessor dp = pf.onPath("/").withValidator(vf.make(new Properties()));

    java.io.File file = getResource("/test/api/myData.dat");
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      JDOMInfosetOutputter outputter = Infoset.getJDOMInfosetOutputter();
      ParseResult res = dp.parse(dis, outputter);

      assertTrue(res.isValidationError());
    }
  }
}
