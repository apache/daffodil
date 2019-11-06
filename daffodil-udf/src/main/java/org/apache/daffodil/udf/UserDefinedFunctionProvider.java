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

package org.apache.daffodil.udf;

/**
 * Abstract class used by ServiceLoader to poll for UDF providers on classpath.
 *
 * Through this class, several User Defined Functions can be made available to
 * Daffodil via a single entry in the META-INF/services file.
 *
 * UDF Providers must subclass this, and must initialize the
 * userDefinedFunctionClasses array with all the UDF classes it is providing.
 *
 * If the UDFs being provided have constructors with arguments, the provider
 * subclass must also implement the createUserDefinedFunction to return an
 * initialized function class object based on the supplied namespace and name.
 *
 * Subclasses must also supply a
 * src/META-INF/services/org.apache.daffodil.udf.UserDefinedFunctionProvider
 * file in their JAVA project in order to be discoverable by Daffodil.
 *
 */

public abstract class UserDefinedFunctionProvider {
  /**
   * Must be implemented to return the classes of the User Defined Function this
   * provider is aware of/providing
   *
   * @return array of the different UserDefinedFunction classes it provides
   */
  abstract public Class<?>[] getUserDefinedFunctionClasses();

  /**
   * Finds and initializes User Defined Function class based on namespace and name
   * provided. The UserDefinedFunctionIdentification annotation applied to the
   * function class must match name and namespaceURI field passed in from the
   * schema.
   *
   * Must be overloaded if the function class's constructor takes arguments.
   * Otherwise it will throw exceptions.
   *
   * @param namespaceURI
   *          XML namespace associated with schema function call
   * @param fName
   *          function name called in schema
   * @return initialized UserDefinedFunction object that must contain evaluate
   *         function with desired functionality
   *
   * @throws SecurityException
   *           if security manager exists and disallows access
   * @throws IllegalArgumentException
   *           if the UDF doesn't have a no-argument constructor
   * @throws ExceptionInInitializerError
   *           if there is an issue initializing the UDF object
   * @throws ReflectiveOperationException
   *           if the UDF doesn't have a no-argument constructor or if there is an
   *           issue initializing the UDF object
   */
  public UserDefinedFunction createUserDefinedFunction(String namespaceURI, String fName)
      throws IllegalArgumentException, SecurityException, ExceptionInInitializerError,
      ReflectiveOperationException {
    UserDefinedFunction fcObject = null;
    for (Class<?> udfc : getUserDefinedFunctionClasses()) {
      UserDefinedFunctionIdentification fInfo = udfc
          .getAnnotation(UserDefinedFunctionIdentification.class);
      String ns = fInfo.namespaceURI();
      String fname = fInfo.name();
      if (namespaceURI.equals(ns) && fName.equals(fname)) {
        fcObject = (UserDefinedFunction) udfc.getConstructor().newInstance();
        break;
      }
    }
    return fcObject;
  }
}
