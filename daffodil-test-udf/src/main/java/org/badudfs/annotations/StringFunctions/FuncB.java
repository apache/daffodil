package org.badudfs.annotations.StringFunctions;

import java.io.Serializable;

public class FuncB implements Serializable {
	public Boolean evaluate(String str1, String str2) {
		Boolean ret = false;
		ret = str1.contentEquals(str2);
		return ret;
	}
}
