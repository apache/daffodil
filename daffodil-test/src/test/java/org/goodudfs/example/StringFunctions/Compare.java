package org.goodudfs.example.StringFunctions;

import java.io.Serializable;

import org.apache.daffodil.udf.FunctionClassInfo;

@FunctionClassInfo(
		name = "compare",
		namespace = "http://goodudfs.StringFunctions"
)
public class Compare implements Serializable {
	private static final long serialVersionUID = -2258860835472436275L;

	public Boolean evaluate(String str1, String str2) {
		Boolean ret = false;
		ret = str1.contentEquals(str2);
		return ret;
	}
}
