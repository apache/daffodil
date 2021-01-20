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

import javax.xml.transform.dom.DOMResult
import javax.xml.transform.dom.DOMSource
import java.io.InputStream
import javax.xml.transform.Source
import javax.xml.transform.Templates
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.TransformerFactory
import org.apache.daffodil.validation.schematron.Schematron.templatesRootDir

import java.nio.file.Path

object Transforms {
  def from(sch: InputStream, srcfmt: SchSource, tf: TransformerFactory): Templates = read(sch, srcfmt.stages, tf)

  private def read(sch: InputStream, stages: Seq[String], tf: TransformerFactory): Templates = tf.newTemplates(
    stages.foldLeft(new StreamSource(sch): Source) { (source, template) =>
      val xsl = getClass.getClassLoader.getResourceAsStream(s"$templatesRootDir/$template")
      val result: DOMResult = new DOMResult
      tf.newTransformer(new StreamSource(xsl)).transform(source, result)
      new DOMSource(result.getNode)
    }
  )
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
    lazy val stages = List("iso_dsdl_include.xsl", "iso_abstract_expand.xsl", "iso_svrl_for_xslt2.xsl")
  }
  case object Xsd extends SchSource {
    lazy val stages: Seq[String] = "ExtractSchFromXSD-2.xsl" :: Sch.stages
  }
}
