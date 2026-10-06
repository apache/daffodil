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

package org.apache.daffodil.api.infoset;

/**
 * XMLTextLineEndingStyles determine how carriage return (CR) and line feed (LF)
 * characters in the text of xs:string elements are represented in an XML infoset.
 * <p>
 * The XML specification requires parsers to convert CRLF and lone CR to LF, which
 * loses information about the original line endings. Only the Normalize style is
 * lossy. The other styles preserve line endings exactly when the same style is
 * used to read the infoset back in.
 */
public enum XMLTextLineEndingStyle {
  /**
   * CRLF and a lone CR are converted to LF, which is standard XML behavior. This is
   * the default.
   */
  Normalize,

  /**
   * CR is remapped to the Unicode Private Use Area character U+E00D. LF is not changed,
   * so CRLF is represented as U+E00D followed by LF.
   */
  PUA,

  /**
   * CR is represented as the two characters backslash and 'r', LF as backslash and
   * 'n', and a backslash as two backslashes, similar to JSON string escaping. The
   * resulting text contains no CR or LF characters.
   */
  Escape;
}
