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

package org.apache.daffodil.infoset

import org.junit.Test
import org.apache.daffodil.infoset.EXIInfosetInputter
import org.apache.daffodil.api
import java.io.FileInputStream
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.api.DaffodilSchemaSource

class TestEXIInfoset {

// The application shall support input EXI files
@Test def test_supportEXIFiles() = {
    //open exi file and show returned object is EXIInfosetInputter
}

// The application shall support EXI files created with the EXIficient library
@Test def test_supportEXIficient() = {
    //exiinfosetinputter with this lib

    //load input file made with this lib

    //parse with selected lib

}
// The application shall support EXI files created with the AgileDelta EXI library
@Test def test_supportAgileDelta() = {
    //exiinfosetinputter with this lib

    //load input file made with this lib

    //parse with selected lib
    
}
// The application shall support EXI files created with the Nagasena EXI library
@Test def test_supportNagasena() = {
    //exiinfosetinputter with this lib

    //load input file made with this lib

    //parse with selected lib
    
}
// The application shall have the ability to support unnamed libraries REWORD
//???
// The application shall support bit packed representation of infosets
@Test def test_supportBitPackedRepresentation() = {
    //exiinfosetinputter with this lib
    
}
// The application shall support compression of infosets
@Test def test_supportCompressionRepresentation() = {

}
// The application shall support the option for namespace preservation
@Test def test_supportNamespace() = {

}
// The application shall allow the user to specify the INCLUDE_OPTIONS
@Test def test_IncludeOptions() = {

}
// The application shall allow the user to specify the INCLUDE_SCHEMA_ID
@Test def test_IncludeSchemaID() = {

}
// The application shall allow the user to preserve processed schemas 
@Test def test_preserveSchema() = {
    //parse schema
    val isss = new InputStreamSchemaSource(EXIInfosetInputter.ConvertEXIToXMLWithExificient(
        new FileInputStream("output/customerInformedExificient.dfdl.xsd.exi")))
    val c = Compiler(validateDFDLSchemas = false)
    val pf = c.compileSource(isss, Some("list"), None)
    val dp = pf.onPath("/")

    val stii = new XMLTextInfosetInputter(new FileInputStream("input9.txt.xml"))
    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    dp.parse(stii, wbc)
    System.err.println(bos.toString)

    //do work with translated schema
}
// The application shall allow the user to preserve initialized Inputters
// The application shall allow the user to preserve initialized Outputters

}
