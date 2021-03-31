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

import org.apache.daffodil.debugger.Debugger;
import org.apache.daffodil.japi.Daffodil;
import org.apache.daffodil.japi.DataProcessor;
import org.apache.daffodil.japi.ParseResult;
import org.apache.daffodil.japi.ProcessorFactory;
import org.apache.daffodil.japi.infoset.NullInfosetOutputter;
import org.apache.daffodil.japi.io.InputSourceDataInputStream;
import org.apache.daffodil.processors.parsers.PState;
import org.apache.daffodil.processors.parsers.Parser;
import org.apache.daffodil.util.Misc;
import org.junit.Test;

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
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();

        CustomDebugger dbg = new CustomDebugger();
        URI schemaFileName = Misc.getRequiredResource("/test/japi/mySchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileSource(schemaFileName);
        DataProcessor dp = pf.onPath("/")
                .withDebugger(dbg)
                .withDebugging(true);

        String file = Misc.getRequiredResource("/test/japi/myData2.dat").toURL().getFile();
        java.io.FileInputStream fis = new java.io.FileInputStream(file);
        InputSourceDataInputStream dis = new InputSourceDataInputStream(fis);
        ParseResult res = dp.parse(dis, new NullInfosetOutputter());

        assertEquals(6, dbg.nodes);
        assertTrue(dbg.inited);
        assertTrue(dbg.finished);
    }
}
