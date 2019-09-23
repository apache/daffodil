<!--
  Licensed to the Apache Software Foundation (ASF) under one or more
  contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
  The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-->

# User Defined Functions

## Introduction

Apache Daffodil (incubating) allows execution of Java external/user defined functions in DPath Expressions.

## Getting Started

The implementer will be expected to provide a JAR via the classpath containing at least 2 classes: a provider class and its associated serializable function class(es).

The JAR can have as many provider classes as desired as long as the classes are registered in the *services* folder.

### UDF Implementation

#### Provider Classes

The provider class will be an implementation of Daffodil's `UDFuntionProvider` class. This class must initialize its functionClasses array with all the classes it's providing. It must also implement a lookup function that returns an initialized function class object based on a supplied name and namespace. 

This class will act as a traditional service provider as explained in the ServiceLoader API, and must have a *src/META-INF/services folder* in its JAVA project with a file named `org.apache.daffodil.udfs.UDFunctionProvider`. This file must contain the fully qualified name(s) of the provider class(es) in the JAR. Without that file, neither this class nor any of the function classes it provides will be made visible to Daffodil.

The class can provide as many function classes as it wishes, it only has to track them in the functionClasses array and initialize them in the lookup function.

#### Function Classes

The function class will contain the functionality of the desired UDF. It must be annotated with the `FunctionClassInfo` annotation class, with its name and namespace filled in. It must also implement a function named *evaluate* that Daffodil will call to execute the desired UDF functionality. There is no support for overloaded evaluate functions.

The function class must be serializable.

### UDF Usage

To use within a DPATH expression, you will need to define a xsd namespace whose value matches the namespace of the function class's annotation. Then you can call the function with the name matching the function class's annotation. For example:

```
<!-- within the schema tag -->
xmlns:sdf="com.ext.UDFunction.StringFunctions"

<!-- within the DPATH expression -->
..."{ sdf:replace(., ' ', '_') }"...
```

# Restrictions

- Overloading unsupported

