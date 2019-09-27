package org.badudfs.evaluate.StringFunctions;

import java.io.Serializable;

import org.apache.daffodil.udf.FunctionClassInfo;

@FunctionClassInfo(
		name = "funcA",
		namespace = "com.ext.badudfs.StringFunctions"
)
public class FuncA implements Serializable {

	public String evaluate(String orig, String pre, String post) {
		String ret = "";
		if (orig.length() >= pre.length() ) {
			ret = orig.replace(pre, post);
		}
		return ret;
	}

	public String evaluate(char[] orig, char[] pre, char[] post) {
		String ret = "";
		if (orig.length >= pre.length ) {
			String newOrig = orig.toString();
			ret = newOrig.replace(pre.toString(), post.toString());
		}
		return ret;
	}
}
