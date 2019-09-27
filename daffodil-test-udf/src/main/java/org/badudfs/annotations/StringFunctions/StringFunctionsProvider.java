package org.badudfs.annotations.StringFunctions;

import org.apache.daffodil.udf.*;

public class StringFunctionsProvider extends UDFunctionProvider {
	public StringFunctionsProvider() {
		super.setFunctionClasses( new Class<?>[] { Compare.class, FuncB.class } );
	}

	public Object lookupFunctionClass(String namespace, String name) {
		Object functionClass = null;

		String nn = String.join("_", namespace, name);

		switch (nn) {
		case "":
			functionClass = new Compare();
			break;
		default:
			functionClass = new FuncB();
			break;
		}
		return functionClass;
	}
}
