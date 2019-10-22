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

Apache Daffodil (incubating) allows execution of Java/Scala external/user defined functions in DPath Expressions.

## Getting Started

The implementer will be expected to provide a JAR via the classpath containing at least 2 classes: a provider class and its associated function class(es).

The JAR can have as many provider classes as desired as long as the classes are registered in the *services* folder.

### UDF Implementation

#### User Defined Function Provider Classes

The provider class must extend Daffodil's `UserDefinedFuntionProvider` class. This class must implement the `getUserDefinedFunctionClasses` abstract method, and ensure it returns all the User Defined Functions this class is providing. The `UserDefinedFunctionProvider` class provides a `createUserDefinedFunction` to lookup and initialize User Defined Functions that have no-argument constructors, based on a supplied name and namespaceURI. If the User Defined Function has a constructor that takes arguments, then the default lookup function cannot be used, and the class must override the look up function with its own implementation for such UDFs.

This class will act as a traditional service provider as explained in the ServiceLoader API, and must have a *META-INF/services/org.apache.daffodil.udf.UserDefinedFunctionProvider* file in its project. This file must contain the fully qualified name(s) of the **provider class(es)** in the JAR. Without that file, neither this class nor any of the User Defined Function classes it provides will be visible to Daffodil.

The class can provide as many User Defined Function classes as it wishes, as long as they are made available by the `getUserDefinedFunctionClasses` function.

#### User Defined Function Classes

The User Defined Function class must extend Daffodil's `UserDefinedFunction` class. This class will contain the actual functionality implementers wish to add to DPath Expressions. It must be annotated with the `UserDefinedFunctionIdentification` annotation class, with the name and namespaceURI fields filled in with how the UDF will be called from the schema. It must also implement a function named *evaluate* that Daffodil will call to execute the desired UDF functionality. *There is no support for overloaded or void evaluate functions*.

### UDF Usage

To use within a DPATH expression, you will need to either define an xsd namespace or set as your default namespace a value that matches the namespaceURI of the UDF's annotation. Then you can call the function with the name field of the function class's annotation. For example:

```xml
<!--
For a UDF with the following annotation
@UserDefinedFunctionIdentification(
    name = "replace",
    namespaceURI = "http://ext.StringFunctions.com")
-->

<!-- within the schema tag -->
xmlns:sdf="http://ext.StringFunctions.com"

<!-- within the DPATH expression -->
..."{ sdf:replace(., ' ', '_') }"...
```

## Supported Types
* BigDecimal: Java or Scala
* BigInteger: Java or Scala
* Boolean: Boxed and Primitive
* Byte: Boxed, Primitive, Array of Boxed
* Double: Boxed and Primitive
* Float: Boxed and Primitive
* Integer: Boxed and Primitive
* Long: Boxed and Primitive
* Short: Boxed and Primitive
* String
* URI

## Restrictions

- Overloading unsupported
- Void functions unsupported
