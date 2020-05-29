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
package org.apache.daffodil.codegen.python;

import java.util.Objects;

import org.apache.daffodil.codegen.ast.Import;
import org.apache.daffodil.codegen.ast.ImportExtractor;
import org.apache.daffodil.codegen.ast.TypeDefinition;

public class PythonImportExtractor extends ImportExtractor {

  @Override
  protected TypeDefinition visitTypeDefinition(TypeDefinition definition) {
    final String typeString = definition.getTypeString();
    if (typeString.contains(".")) {
      // Add Import Statement
      imports.add(new PythonImport(typeString));

      final String className = typeString.substring(typeString.lastIndexOf(".") + 1);

      return new TypeDefinition(className);
    } else {
      return definition;
    }
  }

  static class PythonImport implements Import {

    private final String identifier;

    PythonImport(String identifier) {
      this.identifier = identifier;
    }

    public String getIdentifier() {
      return identifier;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o)
        return true;
      if (o == null || getClass() != o.getClass())
        return false;
      PythonImport that = (PythonImport) o;
      return Objects.equals(identifier, that.identifier);
    }

    @Override
    public int hashCode() {
      return Objects.hash(identifier);
    }
  }

}
