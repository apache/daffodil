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
public enum ValidationMode {
	Off(10), Limited(20), Full(30);

	public int id;

	private ValidationMode(int id) {
		this.id = id;
	}
}
