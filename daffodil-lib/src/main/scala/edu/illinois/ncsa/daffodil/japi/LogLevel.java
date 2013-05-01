package edu.illinois.ncsa.daffodil.japi;

///**
// * To create enums for Scala that are usable from Java, this is the only
// * reasonably robust idiom I could come up with. You basically have to create an
// * enum in java where any constant values are defined, then use those in the
// * Scala code to initialize its enum idiom.
// * 
// */
/**
 * Logging levels.
 * <p>
 * Error, Warning, and Info are intended for general use. The default is Info.
 * <p>
 * Levels Compile, Debug, and OOLAGDebug are intended for Daffodil developer
 * use.
 * 
 */
public enum LogLevel {
	Error(10), Warning(20), Info(30), Compile(40), Debug(50), OOLAGDebug(60);

	public int id;

	private LogLevel(int id) {
		this.id = id;
	}
}
