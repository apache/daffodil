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

import java.util.Arrays;
import java.util.List;
import java.util.ServiceConfigurationError;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.ServiceLoader;
import java.util.stream.Collectors;

import org.apache.daffodil.util.Misc;

public class UDFunctionService {
	private ServiceLoader<UDFunctionProvider> loader;
	private ArrayList<Class<?>> functionClasses = new ArrayList<>();
	private HashMap<String, UDFunctionProvider> functionProviderLookup = new HashMap<>();
	private ArrayList<String> errors = new ArrayList<>();
	private ArrayList<String> warnings = new ArrayList<>();

	public UDFunctionService() {
		try {
			loader = ServiceLoader.load(UDFunctionProvider.class);

			loader.forEach(fcp -> {
				Class<?>[] fcpfc = fcp.getFunctionClasses();

				if (fcpfc == null || fcpfc.length == 0) {
					warnings.add(String.format("Provider ignored: [%s].\nNo Function Classes found.",
							fcp.getClass().getName()));
					return;
				}

				List<Class<?>> fc = Arrays.asList(fcpfc);

				List<Class<?>> missingAnnotations = fc.stream()
						.filter(f -> !f.isAnnotationPresent(FunctionClassInfo.class)).collect(Collectors.toList());

				List<Class<?>> nonSerializables = fc.stream().filter(f -> Serializable.class.isAssignableFrom(f))
						.collect(Collectors.toList());

				if (!missingAnnotations.isEmpty()) {
					warnings.add(
							String.format("Provider ignored: [%s].\nAnnotations missing for FunctionClass(es):\n%s",
									fcp.getClass().getName(), missingAnnotations.stream().map(c -> c.getName())
											.collect(Collectors.joining("\n", "\t[", "]"))));
					return;
				}

				if (!nonSerializables.isEmpty()) {
					warnings.add(String.format(
							"Provider ignored: [%s].\nFunctionClass(es) must implement java.io.Serializable:\n%s",
							fcp.getClass().getName(), nonSerializables.stream().map(c -> c.getName())
									.collect(Collectors.joining("\n", "\t[", "]"))));
					return;
				}

				fc.stream().forEach(f -> {
					FunctionClassInfo fInfo = f.getAnnotation(FunctionClassInfo.class);
					String ns = fInfo.namespace();
					String fname = fInfo.name();
					if (ns.isEmpty() || fname.isEmpty() || ns.matches("\\s+") || fname.matches("\\s+")) {
						warnings.add(String.format("FunctionClass ignored: [%s].%s%s", f.getName(),
								ns.isEmpty() ? "\nAnnotation namespace field is empty or invalid." : "",
								fname.isEmpty() ? "\nAnnotation name field is empty or invalid." : ""));
					}

					String key = String.join("_", fInfo.namespace(), fInfo.name());
					if (functionProviderLookup.containsKey(key)) {
						warnings.add(String.format(
								"FunctionClass ignored: [%s].\nDuplicate name [%s] and namespace[%s] found.",
								f.getName(), key.split("_")[0], key.split("_")[1]));
					} else {
						functionProviderLookup.put(key, fcp);
						functionClasses.add(f);
					}
				});
			});

			if (functionClasses.isEmpty()) {
				errors.add(String.format("No user defined functions found."
						+ "\nCheck that UDF JARs are on classpath and that they are properly registerable by ServiceLoader."
						+ "\nCurrent classpath locations:\n%s",
						Arrays.asList(Misc.getClassPath()).stream().map(u -> u.toString())
								.collect(Collectors.joining("\n"))));
			}
		} catch (ServiceConfigurationError | Exception e) {
			e.printStackTrace();
			errors.add(e.toString() + String.format("\nCurrent classpath locations:\n%s", Arrays
					.asList(Misc.getClassPath()).stream().map(u -> u.toString()).collect(Collectors.joining("\n"))));
		}
	}

	public Object lookupFunctionClass(String namespace, String name) {
		Object funcObj = null;

		try {
			UDFunctionProvider fp = this.functionProviderLookup.get(String.join("_", namespace, name));
			if (fp != null) {
				funcObj = fp.lookupFunctionClass(namespace, name);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return funcObj;
	}

	public ArrayList<Class<?>> getFunctionClasses() {
		return this.functionClasses;
	}

	public ArrayList<String> getWarnings() {
		return this.warnings;
	}

	public ArrayList<String> getErrors() {
		return this.errors;
	}

}
