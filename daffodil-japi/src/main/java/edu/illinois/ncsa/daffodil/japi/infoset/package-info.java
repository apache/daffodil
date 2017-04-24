/**
 * Defines various classes used control the representation of the infoset for
 * parse and unparse. Classes that extend {@link edu.illinois.ncsa.daffodil.japi.infoset.InfosetOutputter} are provided to
 * the {@link edu.illinois.ncsa.daffodil.japi.DataProcessor#parse(java.nio.channels.ReadableByteChannel, edu.illinois.ncsa.daffodil.japi.infoset.InfosetOutputter, long)} method to deteremine how to output an infoset.
 * These classes are not guaranteed to be thread-safe. Classes that extend
 * {@link edu.illinois.ncsa.daffodil.japi.infoset.InfosetInputter} are provided to the {@link edu.illinois.ncsa.daffodil.japi.DataProcessor#unparse(edu.illinois.ncsa.daffodil.japi.infoset.InfosetInputter, java.nio.channels.WritableByteChannel)} method to
 * determine how to read in an infoset. A new InfosetOutputter is required for
 * each call to unparse().
 */

package edu.illinois.ncsa.daffodil.japi.infoset;
