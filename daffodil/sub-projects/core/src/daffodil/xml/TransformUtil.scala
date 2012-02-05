/**
 * University of Illinois/NCSA
 * Open Source License
 *  
 * Copyright (c) 2010, NCSA.  All rights reserved.
 *  
 * Developed by:
 * Cyberenvironments and Technologies (CET)
 * http://cet.ncsa.illinois.edu/
 *  
 * National Center for Supercomputing Applications (NCSA)
 * http://www.ncsa.illinois.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the 
 * "Software"), to deal with the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *  
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimers.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimers in the
 *   documentation and/or other materials provided with the distribution.
 * - Neither the names of CET, University of Illinois/NCSA, nor the names
 *   of its contributors may be used to endorse or promote products
 *   derived from this Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF 
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
 *******************************************************************************/package daffodil.xml

import java.io.{File, FileInputStream}
import javax.xml.transform.{Transformer, TransformerFactory, OutputKeys, Source, Result}
import javax.xml.transform.stream.{StreamSource, StreamResult}
import java.io.{InputStream, OutputStream}

object TransformUtil {
	// catch and ignore all exceptions
	private def catchall(op: => Unit) {
		try {
			op
		} catch {
			case ex:Exception =>
		}
	}
	
	def transformer(xf:Source):Transformer = {
		val tf:TransformerFactory = TransformerFactory.newInstance()
		catchall { tf.setAttribute("indent-number", 2) }
		val t:Transformer = tf.newTransformer(xf)
		catchall { t.setOutputProperty(OutputKeys.INDENT, "yes") }
		catchall { t.setOutputProperty(OutputKeys.METHOD, "xml") }
		catchall { t.setOutputProperty("{http://xml.apache.org/xslt}indent-amount","2") }
		t
	}
	
	def transform(input:Source, xf:Source, result:Result) {
		transformer(xf).transform(input, result)
	}
	
	def transform(input:InputStream, xf:InputStream, result:OutputStream) {
		transformer(new StreamSource(xf)).transform(new StreamSource(input), new StreamResult(result))
	}
}