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

import javax.xml.transform.Source
import javax.xml.transform.URIResolver
import javax.xml.transform.stream.StreamSource
import org.apache.daffodil.api.ValidatorInitializationException

/**
 * Class path resolver for schematron templates
 */
final class ClassPathUriResolver(rulesDir: String, fallback: Option[URIResolver]) extends URIResolver {
  def resolve(href: String, base: String): Source = {
    val path = s"$rulesDir/$href"
    Option(getClass.getClassLoader.getResourceAsStream(path)) match {
      case Some(is) => new StreamSource(is)
      case None =>
        // fallback #1;; try the raw classpath without the prefix
        Option(getClass.getClassLoader.getResourceAsStream(href)) match {
          case Some(is) => new StreamSource(is)
          case None =>
            // fallback #2;; use the fallback resolver if provided
            fallback.map(_.resolve(href, base)).getOrElse(
              throw ValidatorInitializationException(s"schematron resource not found at $path")
            )
        }
    }
  }
}

object ClassPathUriResolver {
  def apply(rulesDir: String): ClassPathUriResolver =
    new ClassPathUriResolver(rulesDir, None)

  def apply(rulesDir: String, fallback: URIResolver): ClassPathUriResolver =
    new ClassPathUriResolver(rulesDir, Some(fallback))
}
