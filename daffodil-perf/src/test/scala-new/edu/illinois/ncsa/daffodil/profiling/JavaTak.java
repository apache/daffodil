package edu.illinois.ncsa.daffodil.profiling;

class JavaTak {

	static void calibrate() {
		if (takeons == 0.0) {
			testTak();
		}
	}

	static double takeons = 0.0;

	static long callCount = 0;

	// Original Tak function
	static int tak(int x, int y, int z) {
		callCount += 1;
		if (y < x) {
			return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y));
		} else {
			return z;
		}
	}

	static void testTak() {
		System.out.println("JavaTak: Calibrating takeon units");
		callCount = 0;
		long t0 = System.nanoTime();
		tak(21, 3, 21);
		long t1 = System.nanoTime();
		long nanos = t1 - t0;
		System.out.println("tak call count = " + callCount + " in " + nanos
				+ "ns");
		takeons = (1.0 * nanos) / callCount;
		System.out.println("Under current load, 1 CPU of this system executes "
				+ takeons + " nanoseconds per tak call.");
		System.out.println("So on this system, currently, 1 takeon = "
				+ takeons + "ns");
		System.out.println("Done calibrating");
	}

}
