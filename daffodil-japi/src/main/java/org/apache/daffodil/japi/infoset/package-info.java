/**
 * Defines various classes used control the representation of the infoset for
 * parse and unparse. Classes that extend {@link org.apache.daffodil.japi.infoset.InfosetOutputter} are provided to
 * the {@link org.apache.daffodil.japi.DataProcessor#parse(java.nio.channels.ReadableByteChannel, org.apache.daffodil.japi.infoset.InfosetOutputter, long)} method to deteremine how to output an infoset.
 * These classes are not guaranteed to be thread-safe. Classes that extend
 * {@link org.apache.daffodil.japi.infoset.InfosetInputter} are provided to the {@link org.apache.daffodil.japi.DataProcessor#unparse(org.apache.daffodil.japi.infoset.InfosetInputter, java.nio.channels.WritableByteChannel)} method to
 * determine how to read in an infoset. A new InfosetOutputter is required for
 * each call to unparse().
 */

package org.apache.daffodil.japi.infoset;
