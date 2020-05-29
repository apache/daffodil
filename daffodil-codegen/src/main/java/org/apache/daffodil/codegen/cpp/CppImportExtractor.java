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
package org.apache.daffodil.codegen.cpp;

import java.util.Objects;

import org.apache.daffodil.codegen.ast.Import;
import org.apache.daffodil.codegen.ast.ImportExtractor;
import org.apache.daffodil.codegen.ast.Primitive;
import org.apache.daffodil.codegen.ast.TypeDefinition;

public class CppImportExtractor extends ImportExtractor {

  @Override
  protected TypeDefinition visitTypeDefinition(TypeDefinition definition) {
    final String typeString = definition.getTypeString();
    if (typeString.contains(".")) {
      // Add Import Statement
      imports.add(new CppPackageImport(typeString, ImportType.USE));

      final String className = typeString.substring(typeString.lastIndexOf(".") + 1);

      return new TypeDefinition(className);
    } else if (definition instanceof Primitive) {
      imports.add(new CppPackageImport("<iostream>", ImportType.INCLUDE));
      imports.add(new CppPackageImport("std", ImportType.USE));
      return definition;
    } else {
      return definition;
    }
  }

  public static class CppPackageImport implements Import {

    private final String typeString;
    private final ImportType type;

    public CppPackageImport(String typeString, ImportType type) {
      this.typeString = typeString;
      this.type = type;
    }

    public String getTypeString() {
      return typeString;
    }

    public ImportType getType() {
      return type;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o)
        return true;
      if (o == null || getClass() != o.getClass())
        return false;
      CppPackageImport that = (CppPackageImport) o;
      return Objects.equals(typeString, that.typeString) && type == that.type;
    }

    @Override
    public int hashCode() {
      return Objects.hash(typeString, type);
    }
  }

  public enum ImportType {
    INCLUDE, USE
  }
}
