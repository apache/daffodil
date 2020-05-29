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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public abstract class ImportExtractor implements NodeVisitor<Node> {

  protected final Set<Import> imports = new HashSet<>();

  public List<Import> getImports() {
    return new ArrayList<>(imports);
  }

  protected abstract TypeDefinition visitTypeDefinition(TypeDefinition definition);

  @Override
  public Node visit(ConstantExpression constantExpression) {
    return constantExpression;
  }

  @Override
  public Node visit(DeclarationStatement declarationStatement) {
    return new DeclarationStatement(((ParameterExpression) declarationStatement.getParameterExpression().accept(this)),
        ((Expression) declarationStatement.getInitializer().accept(this)));
  }

  @Override
  public Node visit(ParameterExpression parameterExpression) {
    return new ParameterExpression(visitTypeDefinition(parameterExpression.getType()), parameterExpression.getName());
  }

  @Override
  public Node visit(Block block) {
    return new Block(block.getStatements().stream().map(stmt -> stmt.accept(this)).collect(Collectors.toList()));
  }

  @Override
  public Node visit(ClassDeclaration classDeclaration) {
    return new ClassDeclaration(classDeclaration.getNamespace(), classDeclaration.getClassName(),
        classDeclaration.getFields().stream().map(field -> (FieldDeclaration) field.accept(this))
            .collect(Collectors.toList()),
        classDeclaration.getConstructors(), classDeclaration.getMethods().stream()
            .map(method -> (MethodDefinition) method.accept(this)).collect(Collectors.toList()),
        classDeclaration.getInnerClasses());
  }

  @Override
  public Node visit(IfStatement ifStatement) {
    return new IfStatement(
        ifStatement.getConditions().stream().map(node -> (Expression) node.accept(this)).collect(Collectors.toList()),
        ifStatement.getBlocks().stream().map(node -> (Block) node.accept(this)).collect(Collectors.toList()));
  }

  @Override
  public Node visit(MethodDefinition methodDefinition) {
    return new MethodDefinition(methodDefinition.getModifiers(), methodDefinition.getName(),
        visitTypeDefinition(methodDefinition.getResultType()), methodDefinition.getParameters().stream()
            .map(param -> ((ParameterExpression) param.accept(this))).collect(Collectors.toList()),
        ((Block) methodDefinition.getBody().accept(this)));
  }

  @Override
  public Node visit(BinaryExpression binaryExpression) {
    return binaryExpression;
  }

  @Override
  public Node visit(AssignementExpression assignementExpression) {
    return new AssignementExpression(((Expression) assignementExpression.getTarget().accept(this)),
        assignementExpression.getValue().accept(this));
  }

  @Override
  public Node visit(CallExpression callExpression) {
    return callExpression;
  }

  @Override
  public Node visit(FieldDeclaration fieldDeclaration) {
    Expression initializer = fieldDeclaration.getInitializer();

    return new FieldDeclaration(fieldDeclaration.getModifiers(), visitTypeDefinition(fieldDeclaration.getType()),
        fieldDeclaration.getName(),
        (initializer != null) ? ((Expression) fieldDeclaration.getInitializer().accept(this)) : null);
  }

  @Override
  public Node visit(FileDeclaration fileDeclaration) {
    return new FileDeclaration(fileDeclaration.getImports(),
        ((ClassDeclaration) fileDeclaration.getClassDeclaration().accept(this)));
  }
}
