/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.util

//
// Copyright (C) 2011, 2012 by Michael J. Beckerle, All rights Reserved.
// Permission is granted to use this software for any purpose so long as
// this copyright is preserved in both the source and binary forms, and
// in any documentation provided with the software.
//

import javax.xml.transform.stream.StreamSource
import javax.xml.XMLConstants
import javax.xml.validation.SchemaFactory
import scala.xml.parsing.NoBindingFactoryAdapter
import scala.xml._
import java.io.StringReader
import java.net.URI
import edu.illinois.ncsa.daffodil.xml.DFDLCatalogResolver
import scala.collection.mutable
import org.xml.sax.ErrorHandler

/**
 * Use this for extra validation passes in the TDML Runner
 * to do a validation pass on the TDML expected Infoset w.r.t. the model and to
 * do a validation pass on the actual result w.r.t. the model as an XML document.
 */
object Validator extends NoBindingFactoryAdapter {

  private type CacheType = mutable.HashMap[Seq[String], javax.xml.validation.Validator]

  private val validationSchemaCache =
    new ThreadLocal[CacheType] {
      override def initialValue =
        new CacheType
    }

  def validateXMLSources(schemaFileNames: Seq[String], document: Node, errHandler: ErrorHandler): Unit = {
    val cache = validationSchemaCache.get()
    val validator = {
      val optCachedValidator = cache.get(schemaFileNames)
      optCachedValidator match {
        case Some(validator) => {
          validator.reset()
          validator
        }
        case None => {
          val schemaSources: Seq[javax.xml.transform.Source] = schemaFileNames.map { fn =>
            {
              val uri = new URI(fn)
              val is = uri.toURL.openStream()
              val stream = new StreamSource(is)
              stream.setSystemId(uri.toString) // must set this so that relative URIs will be created for import/include files.
              stream
            }
          }

          val schemaLang = "http://www.w3.org/2001/XMLSchema"
          val factory = SchemaFactory.newInstance(schemaLang)
          //          val hdlr = new ContentHandler()
          factory.setErrorHandler(errHandler)
          val resolver = DFDLCatalogResolver.get
          factory.setResourceResolver(resolver)
          val schema = factory.newSchema(schemaSources.toArray)
          val validator = schema.newValidator()
          validator.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
          //
          validator.setFeature("http://xml.org/sax/features/validation", true)

          // If you enable the feature below, it seems to do no validation at all. Just passes.
          //          validator.setFeature("http://apache.org/xml/features/validation/dynamic", true)

          validator.setFeature("http://apache.org/xml/features/validation/schema", true)
          validator.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
          validator.setErrorHandler(errHandler)
          validator.setResourceResolver(resolver)
          cache.put(schemaFileNames, validator)
          validator
        }
      }
    }
    val documentSource = new StreamSource(new StringReader(
      document.toString() //
      ))
    validator.validate(documentSource)
  }
}

