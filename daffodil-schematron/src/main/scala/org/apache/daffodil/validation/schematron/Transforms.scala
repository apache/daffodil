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

package org.apache.daffodil.validation.schematron

import java.io.InputStream
import java.nio.file.Path
import javax.xml.transform.ErrorListener
import javax.xml.transform.Source
import javax.xml.transform.Templates
import javax.xml.transform.TransformerException
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMResult
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamSource

import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.validation.schematron.Schematron.templatesRootDir

class TransformErrorListener extends ErrorListener {
  override def error(e: TransformerException): Unit = throw e
  override def fatalError(e: TransformerException): Unit = throw e
  override def warning(e: TransformerException): Unit = throw e
}

object Transforms {
  def from(
    sch: InputStream,
    schId: String,
    srcfmt: SchSource,
    tf: TransformerFactory,
  ): Templates =
    read(sch, schId, srcfmt.stages, tf)

  private def read(
    sch: InputStream,
    schId: String,
    stages: Seq[String],
    tf: TransformerFactory,
  ): Templates = {
    val schSource = new StreamSource(sch)
    schSource.setSystemId(schId)

    val templateSource = stages.foldLeft(schSource: Source) { (source, template) =>
      val xslUri = Misc.getRequiredResource(s"$templatesRootDir/$template")
      val xslSource = new StreamSource(xslUri.toURL.openStream)
      xslSource.setSystemId(xslUri.toString)

      val result = new DOMResult
      val t = tf.newTransformer(xslSource)
      t.setErrorListener(new TransformErrorListener)
      t.transform(source, result)

      val newSource = new DOMSource(result.getNode)
      newSource.setSystemId(schId)
      newSource
    }

    tf.newTemplates(templateSource)
  }
}

sealed trait SchSource {
  def stages: Seq[String]
}
object SchSource {
  def from(p: Path): SchSource = p.getFileName.toString.split("\\.").last match {
    case "sch" => Sch
    case _ => Xsd
  }

  case object Sch extends SchSource {
    lazy val stages =
      List("iso_dsdl_include.xsl", "iso_abstract_expand.xsl", "iso_svrl_for_xslt2.xsl")
  }
  case object Xsd extends SchSource {
    lazy val stages: Seq[String] = "ExtractSchFromXSD-2.xsl" :: Sch.stages
  }
}
