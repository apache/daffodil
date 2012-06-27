package daffodil.util

import junit.framework.Assert._ ;
import org.scalatest.junit.JUnit3Suite ;
import daffodil.exceptions._

class MyClass extends Logging {
  
  lazy val msg = {
    System.err.println("computing the message string.")
    "Message %s"
  }
  
  lazy val argString = {
    System.err.println("computing the argument string.")
    "about nothing at all."
  }
  
  def logSomething() {
    setLoggingLevel(LogLevel.Error)
    setLogWriter(ForUnitTestLogWriter)
    System.out.println("before first logDebug call")
    
    // alas, no by-name passing of var-args. 
    // so instead, we pass by name, a by-name/lazy constructed tuple
    // instead.
    
    // Illustrates that our Glob object, because it is passed by name,
    // does NOT force evaluation of the pieces that go into it.
    // So it really makes the whole system behave like it was entirely lazy.
    log(Debug(msg, "number 1"))  // Won't show up in log.
    System.out.println("after first logDebug call")
    System.out.println("before first logError call")
    log(Error(msg, argString)) // Will show up in log.
    System.out.println("after first logError call")
  }
}

class TestLogger extends JUnit3Suite {

  def test1() {
    val c = new MyClass
    c.logSomething()
    Console.out.flush()
    val fromLog = ForUnitTestLogWriter.loggedMsg
    assertEquals("Message about nothing at all.", fromLog)
  }

}