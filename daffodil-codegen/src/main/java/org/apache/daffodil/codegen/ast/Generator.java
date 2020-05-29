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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public abstract class Generator {

    protected final WriterFactory factory;
    protected final List<OutputFile> files;

    protected Generator(WriterFactory factory) {
        this.factory = factory;
        files = new ArrayList<>();
    }

    public List<OutputFile> generateOutput(Node root) {
        prepare(root).write(this);
        return files;
    }

    /**
     * Do preliminary stuff.
     * @param root .
     * @return .
     */
    abstract public Node prepare(Node root);

    protected Node prepareInternal(Node root, ImportExtractor importExtractor) {
        root = root.accept(importExtractor);
        List<Import> imports = importExtractor.getImports();

        if (root instanceof ClassDeclaration) {
            return new FileDeclaration(imports, ((ClassDeclaration) root));
        }

        return root;
    }

    abstract public void generate(ConstantExpression constantExpression);

    abstract public void generateDeclarationWithInitializer(DeclarationStatement declarationStatement);

    abstract public void generateDeclaration(DeclarationStatement declarationStatement);

    abstract public void generate(ParameterExpression parameterExpression);

    abstract public void generatePrimitive(Primitive.DataType primitive);

    abstract public void generate(IfStatement ifStatement);

    abstract public void writeBlock(Block statements);

    abstract public void generate(BinaryExpression binaryExpression);

    abstract public void generate(AssignementExpression assignementExpression);

    abstract public void generateStaticCall(Method method, List<Node> constantNode);

    abstract public void generateCall(Node target, Method method, List<Node> constantNode);

    abstract public void generate(NewExpression newExpression);

    abstract public void generate(MethodDefinition methodDefinition);

    abstract public void generateReturn(Expression value);

    abstract public void generateClass(String className, List<FieldDeclaration> fields, List<ConstructorDeclaration> constructors, List<MethodDefinition> methods, List<ClassDeclaration> innerClasses, boolean mainClass);

    abstract public void generateFieldDeclaration(Set<Modifier> modifiers, TypeDefinition type, String name, Expression initializer);

    abstract public void generateFieldReference(TypeDefinition type, String name);

    abstract public void generateConstructor(Set<Modifier> modifiers, String className, List<ParameterExpression> parameters, Block body);

    abstract public void generateFile(List<Import> imports, ClassDeclaration mainClass, List<ClassDeclaration> innerClasses);

    abstract public void generateType(String typeString);

    abstract public void generateComment(String comment);

    abstract public void generateNoOp();
}
