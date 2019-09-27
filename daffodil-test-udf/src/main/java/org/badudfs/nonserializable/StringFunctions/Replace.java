package org.badudfs.nonserializable.StringFunctions;

import java.io.Serializable;

import org.apache.daffodil.udf.FunctionClassInfo;

@FunctionClassInfo(
		name = "replace",
		namespace = "com.ns.badudfs.StringFunctions"
)
public class Replace implements Serializable {
	private static final long serialVersionUID = 2619376314947336164L;

	public String evaluate(String orig, String pre, String post) {
		String ret = "";
		if (orig.length() >= pre.length() ) {
			ret = orig.replace(pre, post);
		}
		return ret;
	}
}
