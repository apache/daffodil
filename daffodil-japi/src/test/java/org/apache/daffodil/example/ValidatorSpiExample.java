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

package org.apache.daffodil.example;

import com.typesafe.config.ConfigFactory;
import org.apache.daffodil.lib.iapi.ValidatorFactory;
import org.apache.daffodil.example.validation.FailingValidator;
import org.apache.daffodil.example.validation.PassingValidator;
import org.apache.daffodil.japi.Daffodil;
import org.apache.daffodil.japi.DataProcessor;
import org.apache.daffodil.japi.ParseResult;
import org.apache.daffodil.japi.ProcessorFactory;
import org.apache.daffodil.japi.infoset.JDOMInfosetOutputter;
import org.apache.daffodil.japi.io.InputSourceDataInputStream;
import org.apache.daffodil.lib.validation.Validators;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class ValidatorSpiExample {
    private java.io.File getResource(String resPath) {
        try {
            return new java.io.File(this.getClass().getResource(resPath).toURI());
        } catch (Exception e) {
            return null;
        }
    }

    @Test
    public void testPassing() throws Exception {
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/mySchema5.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);

        assertTrue(Validators.isRegistered(PassingValidator.name));
        ValidatorFactory vf = Validators.get(PassingValidator.name);
        DataProcessor dp = pf.onPath("/").withValidator(vf.make(ConfigFactory.empty()));

        java.io.File file = getResource("/test/japi/myData5.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        try (InputSourceDataInputStream dis = new InputSourceDataInputStream(fis)) {
            JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
            ParseResult res = dp.parse(dis, outputter);

            assertFalse(res.isValidationError());
        }
    }

    @Test
    public void testFailing() throws Exception {
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        java.io.File schemaFile = getResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);

        assertTrue(Validators.isRegistered(FailingValidator.name));
        ValidatorFactory vf = Validators.get(FailingValidator.name);
        DataProcessor dp = pf.onPath("/").withValidator(vf.make(ConfigFactory.empty()));

        java.io.File file = getResource("/test/japi/myData.dat");
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        try (InputSourceDataInputStream dis = new InputSourceDataInputStream(fis)) {
            JDOMInfosetOutputter outputter = new JDOMInfosetOutputter();
            ParseResult res = dp.parse(dis, outputter);

            assertTrue(res.isValidationError());
        }
    }
}
