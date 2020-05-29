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
package org.apache.daffodil.codegen.ast;

import java.util.Collections;
import java.util.List;

public class FileDeclaration implements Node {

  final private List<Import> imports;
  final private ClassDeclaration classDeclaration;

  public FileDeclaration(List<Import> imports, ClassDeclaration classDeclaration) {
    this.imports = imports;
    this.classDeclaration = classDeclaration;
  }

  public List<Import> getImports() {
    return imports;
  }

  public ClassDeclaration getClassDeclaration() {
    return classDeclaration;
  }

  @Override
  public <T> T accept(NodeVisitor<T> visitor) {
    return visitor.visit(this);
  }

  @Override
  public void write(Generator writer) {
    writer.generateFile(imports, classDeclaration, Collections.emptyList());
  }
}
