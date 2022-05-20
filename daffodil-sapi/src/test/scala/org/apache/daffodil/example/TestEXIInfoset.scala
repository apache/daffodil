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

package org.apache.daffodil.example

import org.junit.Test
import org.apache.daffodil.sapi.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.sapi.infoset.EXIInfosetInputter
//import org.apache.daffodil.sapi.infoset.EXIInfosetOutputter
import org.apache.daffodil.sapi.Daffodil
import java.io.FileInputStream
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
//import java.io.File

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
    val exii = new EXIInfosetInputter(new FileInputStream("daffodil-sapi/src/test/scala/org/apache/daffodil/example/output/customerInformedExificient.dfdl.xsd.exi"))
    val isss = new org.apache.daffodil.api.InputStreamSchemaSource(exii.getInputStream, None, "exiTranslate", "exi")
    val c = Daffodil.compiler()
    val pf = c.compileStreamSource(isss, Some("list"), None)
    val dp = pf.onPath("/")
    val isdii = new InputSourceDataInputStream(new FileInputStream("daffodil-sapi/src/test/scala/org/apache/daffodil/example/input9.txt"))
    val outputter = new ScalaXMLInfosetOutputter()
    dp.parse(isdii, outputter)
    assert(1==1)
    //do work with translated schema
}

//doesnt compile atm
// @Test def test_outputEXI() = {
//     val exio = new EXIInfosetOutputter(new java.io.FileOutputStream("daffodil-sapi/src/test/scala/org/apache/daffodil/example/output/customerInformedExificientOutput.dfdl.xsd.exi"), false)
//     val isss = new org.apache.daffodil.api.InputStreamSchemaSource(
//         new FileInputStream("daffodil-sapi/src/test/scala/org/apache/daffodil/example/output/customerInformedExificient.dfdl.xsd"),
//          None, "exiTranslate", "exi")
//     val c = Daffodil.compiler()
//     val pf = c.compileStreamSource(isss, Some("list"), None)
//     val dp = pf.onPath("/")
//     dp.parse(isss, exio)
//}   

// The application shall allow the user to preserve initialized Inputters
// The application shall allow the user to preserve initialized Outputters

}
