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

import org.apache.daffodil.japi.Daffodil;
import org.apache.daffodil.japi.DataProcessor;
import org.apache.daffodil.japi.Diagnostic;
import org.apache.daffodil.japi.ProcessorFactory;
import org.apache.daffodil.runtime1.iapi.ChoiceMetadata;
import org.apache.daffodil.runtime1.iapi.ComplexElementMetadata;
import org.apache.daffodil.runtime1.iapi.ElementMetadata;
import org.apache.daffodil.runtime1.iapi.Metadata;
import org.apache.daffodil.runtime1.iapi.MetadataHandler;
import org.apache.daffodil.runtime1.iapi.SequenceMetadata;
import org.apache.daffodil.runtime1.iapi.SimpleElementMetadata;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class TestJavaMetadataAPI {

    private java.io.File getResource(String resPath) {
        try {
            return new File(Objects.requireNonNull(getClass().getResource(resPath)).toURI());
        } catch (Exception e) {
            return null;
        }
    }

    public static class GatherMetadata extends MetadataHandler {

        private final List<Metadata> buf = new ArrayList<>();

        public List<Metadata> getResult() {
            List<Metadata> res = new ArrayList<>(buf); // makes a copy
            buf.clear();
            return res;
        }

        @Override
        public void simpleElementMetadata(SimpleElementMetadata m) {
            buf.add(m);
        }

        @Override
        public void startComplexElementMetadata(ComplexElementMetadata m) {
            buf.add(m);
        }

        @Override
        public void endComplexElementMetadata(ComplexElementMetadata m) {
            buf.add(m);
        }

        @Override
        public void startSequenceMetadata(SequenceMetadata m) {
            buf.add(m);
        }

        @Override
        public void endSequenceMetadata(SequenceMetadata m) {
            buf.add(m);
        }

        @Override
        public void startChoiceMetadata(ChoiceMetadata m) {
            buf.add(m);
        }

        @Override
        public void endChoiceMetadata(ChoiceMetadata m) {
            buf.add(m);
        }
    }


    @Test
    public void testMetadataWalkDataWalk01() throws IOException {
        GatherMetadata gatherMetadata = new GatherMetadata();
        org.apache.daffodil.japi.Compiler c = Daffodil.compiler();
        File schemaFile = getResource("/test/japi/metadataTestSchema1.dfdl.xsd");
        ProcessorFactory pf = c.compileFile(schemaFile);
        if (pf.isError()) {
            String diags = pf.getDiagnostics()
                    .stream()
                    .map(Diagnostic::getMessage)
                    .collect(Collectors.joining(System.lineSeparator()));
            fail(diags);
        }
        DataProcessor dp = pf.onPath("/");
        dp.walkMetadata(gatherMetadata);
        List<Metadata> md = gatherMetadata.getResult();
        List<String> mdQNames = md.stream().map(item -> {
            if (item instanceof ElementMetadata) {
                ElementMetadata em = ((ElementMetadata) item);
                String res = em.toQName() + ((em.isArray()) ? "_array" : "");
                return res;
            } else if (item instanceof SequenceMetadata) {
                return "seq";
            } else if (item instanceof ChoiceMetadata) {
                return "cho";
            } else {
                return "";
            }
        }).collect(Collectors.toList());
        assertEquals("[ex:e1, cho, seq, seq, seq, s1_array, seq, cho, ex:e1]", mdQNames.toString());
    }
}
