package org.badudfs.evaluate.StringFunctions;

import java.io.Serializable;

import org.apache.daffodil.udf.FunctionClassInfo;

@FunctionClassInfo(
		name = "replace",
		namespace = "com.ext.badudfs.StringFunctions"
)
public class Replace implements Serializable {
	private static final long serialVersionUID = 2619376314947336164L;

	public String replace(String orig, String pre, String post) {
		String ret = "";
		if (orig.length() >= pre.length() ) {
			ret = orig.replace(pre, post);
		}
		return ret;
	}
}
