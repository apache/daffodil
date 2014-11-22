package edu.illinois.ncsa.daffodil.japi.logger;

/**
 * Logging levels.
 * <p>
 * Error, Warning, and Info are intended for general use. The default is Info.
 * <p>
 * Levels Resolver Compile, Debug, and OOLAGDebug are intended for Daffodil developer
 * use.
 * 
 */
public enum LogLevel {
	Error(10),
	Warning(20),
	Info(30),
	Resolver(35), // For messages about resolving schema locations from namespaces or other.
	Compile(40),
	Debug(50),
	OOLAGDebug(60),
	DelimDebug(70);

	public int id;

	private LogLevel(int id) {
		this.id = id;
	}
}
