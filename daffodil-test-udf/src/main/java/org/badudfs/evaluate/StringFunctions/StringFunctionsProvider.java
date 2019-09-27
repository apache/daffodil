package org.badudfs.evaluate.StringFunctions;

import org.apache.daffodil.udf.*;

public class StringFunctionsProvider extends UDFunctionProvider {
	public StringFunctionsProvider() {
		super.setFunctionClasses( new Class<?>[] { FuncA.class, Replace.class } );
	}

	public Object lookupFunctionClass(String namespace, String name) {
		Object functionClass = null;

		String nn = String.join("_", namespace, name);

		switch (nn) {
		case "com.ext.badudfs.StringFunctions_replace":
			functionClass = new Replace();
			break;
		case "com.ext.badudfs.StringFunctions_funcA":
			functionClass = new FuncA();
			break;
		}
		return functionClass;
	}
}
