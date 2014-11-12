package edu.illinois.ncsa.daffodil.japi;

/**
 * Validation modes for validating the resulting infoset against the DFDL schema
 */
public enum ValidationMode {
	/**
	 * Turn off all validation against the DFDL schema
	 */
	Off(10),

	/**
	 * Perform only facet validation
	 */
	Limited(20),

	/**
	 * Perform full schema validation using Xerces
	 */
	Full(30);

	public int id;

	private ValidationMode(int id) {
		this.id = id;
	}
}
