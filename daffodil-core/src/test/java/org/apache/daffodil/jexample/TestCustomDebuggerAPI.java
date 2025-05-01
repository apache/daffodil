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
import org.apache.daffodil.api.DataProcessor;
import org.apache.daffodil.api.ParseResult;
import org.apache.daffodil.api.compiler.ProcessorFactory;
import org.apache.daffodil.api.debugger.Debugger;
import org.apache.daffodil.api.InputSourceDataInputStream;
import org.apache.daffodil.runtime1.processors.parsers.PState;
import org.apache.daffodil.runtime1.processors.parsers.Parser;
import org.apache.daffodil.lib.util.Misc;
import org.junit.Test;

import org.apache.daffodil.api.infoset.Infoset;

import java.io.IOException;
import java.net.URI;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class TestCustomDebuggerAPI {
  static class CustomDebugger implements Debugger {
    public int nodes;
    public boolean inited;
    public boolean finished;

    @Override
    public void init(PState state, Parser processor) {
      inited = true;
    }

    @Override
    public void before(PState state, Parser processor) {
      nodes += 1;
    }

    @Override
    public void fini(Parser processor) {
      finished = true;
    }
  }

  @Test
  public void testCustomDebugger() throws IOException, ClassNotFoundException {
    org.apache.daffodil.api.Compiler c = Daffodil.compiler();

    CustomDebugger dbg = new CustomDebugger();
    URI schemaFileName = Misc.getRequiredResource("/test/api/mySchema1.dfdl.xsd");
    ProcessorFactory pf = c.compileSource(schemaFileName);
    DataProcessor dp = pf.onPath("/")
        .withDebugger(dbg)
        .withDebugging(true);

    String file = Misc.getRequiredResource("/test/api/myData2.dat").toURL().getFile();
    java.io.FileInputStream fis = new java.io.FileInputStream(file);
    try (InputSourceDataInputStream dis = Infoset.getInputSourceDataInputStream(fis)) {
      ParseResult res = dp.parse(dis, Infoset.getNullInfosetOutputter());

      assertEquals(6, dbg.nodes);
      assertTrue(dbg.inited);
      assertTrue(dbg.finished);
    }
  }
}
