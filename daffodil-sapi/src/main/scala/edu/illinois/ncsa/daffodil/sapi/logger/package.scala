package edu.illinois.ncsa.daffodil.sapi

/**
 * Provides the classes necessary to recieve logging messages from Daffodil.
 *
 * <h3>Overview</h3>
 *
 * Daffodil comes with three prebuilt log writers:
 * <ul>
 *  <li>[[ConsoleLogWriter]] - writes all log messages to stdout</li>
 *  <li>[[FileLogWriter]] - writes all log messages to file</li>
 *  <li>[[NullLogWriter]] - drop all log messages</li>
 *</ul>
 *
 * To use one of these log writers, create and instance of it and pass it to
 * [[Daffodil.setLogWriter]]. For example, to write all logs to {@code /var/log/daffodil.log}:
 *
 * {{{
 * val lw = new FileLogWriter(new File("/var/log/daffodil.log"))
 * Daffodil.setLogWriter(lw)
 * }}}
 *
 * One may also change the log level using [[Daffodil.setLoggingLevel]], which
 * defaults to [[LogLevel#Info]] if not set. For example, to change the log
 * level to [[LogLevel#Warning]]:
 * {{{
 * Daffodil.setLoggingLevel(LogLevel.Warning);
 * }}}
 *
 */
package object logger

