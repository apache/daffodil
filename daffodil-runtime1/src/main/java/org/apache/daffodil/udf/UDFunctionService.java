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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.ServiceLoader;
import java.util.stream.Collectors;

public class UDFunctionService {
	private ServiceLoader<UDFunctionProvider> loader;
	private ArrayList<Class<?>> functionClasses = new ArrayList<>();
	private HashMap<String, UDFunctionProvider> functionProviderLookup = new HashMap<>();

	public UDFunctionService() {
		loader = ServiceLoader.load(UDFunctionProvider.class);

		loader.forEach(fcp -> {
			List<Class<?>> fc = Arrays.asList(fcp.getFunctionClasses());

			List<Class<?>> missingAnnotations = fc.stream().filter(f -> !f.isAnnotationPresent(FunctionClassInfo.class))
					.collect(Collectors.toList());

			if (missingAnnotations.isEmpty()) {
				fc.stream().forEach(f -> {
					FunctionClassInfo fInfo = f.getAnnotation(FunctionClassInfo.class);
					String key = String.join("_", fInfo.namespace(), fInfo.name());
					functionProviderLookup.put(key, fcp);
					functionClasses.add(f);
				});
			} else {
				System.out.println(String.format("[%s] ignored. Annotations missing for FunctionClass(es):\n%s",
						fcp.getClass().getName(), missingAnnotations.stream().map(c -> c.getName())
								.collect(Collectors.joining("\n", "\t[", "]"))));
			}
		});
	}

	public ArrayList<Class<?>> getFunctionClasses() {
		return this.functionClasses;
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

}
