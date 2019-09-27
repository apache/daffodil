package org.badudfs.nonserializable.StringFunctions;

import org.apache.daffodil.udf.FunctionClassInfo;

@FunctionClassInfo(
		name = "funcA",
		namespace = "com.ns.badudfs.StringFunctions"
)
public class FuncA {

	public String evaluate(String orig, String pre, String post) {
		String ret = "";
		if (orig.length() >= pre.length() ) {
			ret = orig.replace(pre, post);
		}
		return ret;
	}
}
