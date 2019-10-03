package org.goodudfs.example.StringFunctions;

import org.apache.daffodil.udf.*;

public class StringFunctionsProvider extends UDFunctionProvider {
	public StringFunctionsProvider() {
		super.setFunctionClasses( new Class<?>[] { Replace.class, Compare.class } );
	}

	public Object lookupFunctionClass(String namespace, String name) {
		Object functionClass = null;

		String nn = String.join("_", namespace, name);

		switch (nn) {
		case "com.ext.UDFunction.StringFunctions_replace":
			functionClass = new Replace();
			break;
		case "com.ext.UDFunction.StringFunctions_compare":
			functionClass = new Compare();
			break;
		}
		return functionClass;
	}
}
