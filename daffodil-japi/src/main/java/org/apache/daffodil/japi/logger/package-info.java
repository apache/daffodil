/**
 * Provides the classes necessary to recieve logging messages from Daffodil.
 *
 * <h3>Overview</h3>
 *
 * Daffodil comes with three prebuilt log writers:
 * <ul>
 *  <li>{@link edu.illinois.ncsa.daffodil.japi.logger.ConsoleLogWriter} - writes all log messages to stdout</li>
 *  <li>{@link edu.illinois.ncsa.daffodil.japi.logger.FileLogWriter} - writes all log messages to file</li>
 *  <li>{@link edu.illinois.ncsa.daffodil.japi.logger.NullLogWriter} - drop all log messages</li>
 *</ul>
 *
 * To use one of these log writers, create and instance of it and pass it to
 * {@link edu.illinois.ncsa.daffodil.japi.Daffodil#setLogWriter}. For example, to write all logs to {@code /var/log/daffodil.log}:
 *
 * <pre>
 * {@code
 * FileLogWriter lw = new FileLogWriter(new File("/var/log/daffodil.log"));
 * Daffodil.setLogWriter(lw);
 * }</pre>
 *
 * One may also change the log level using {@link edu.illinois.ncsa.daffodil.japi.Daffodil#setLoggingLevel}, which defaults to {@link edu.illinois.ncsa.daffodil.japi.logger.LogLevel#Info} if not set. For example, to change the log level to {@link edu.illinois.ncsa.daffodil.japi.logger.LogLevel#Warning}:
 * <pre>
 * {@code
 * Daffodil.setLoggingLevel(LogLevel.Warning);
 * }</pre>
 *
 */

package edu.illinois.ncsa.daffodil.japi.logger;

