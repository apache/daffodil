/*
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
*/
package org.apache.daffodil.codegen.ast;

public interface NodeVisitor<T> {

    T visit(ConstantExpression constantExpression);

    T visit(DeclarationStatement declarationStatement);

    T visit(ParameterExpression parameterExpression);

    T visit(Block block);

    T visit(ClassDeclaration classDeclaration);

    T visit(IfStatement ifStatement);

    T visit(MethodDefinition methodDefinition);

    T visit(BinaryExpression binaryExpression);

    T visit(AssignementExpression assignementExpression);

    T visit(CallExpression callExpression);

    T visit(FieldDeclaration fieldDeclaration);

    T visit(FileDeclaration fileDeclaration);
}
