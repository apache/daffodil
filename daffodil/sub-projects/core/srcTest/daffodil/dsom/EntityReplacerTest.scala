package daffodil.dsom
import org.scalatest.junit.JUnit3Suite
import java.util.regex.Pattern
import java.util.regex.Matcher

class EntityReplacerTest extends JUnit3Suite {
	def testEntityReplacerWorks(){
	  val e = new EntityReplacer
	  
	  //TODO: Construct and add meaningful tests for EntityReplacer
	  
//	  println(e.replaceDfdlEntity("%%SomeRandomText%NUL;SomeMoreText%BEL;AndMoreText%%%LF;YayText!"))
//	  
//	  println(e.replaceHex("SomeRandomText%#x0000;MoreRandomText%#x000D;NEWline"))
//	  
//	  println(e.replaceDecimal("SomeRandomText%#9;MoreRandomText%#13;NEWline"))
	  
	  println(e.replaceAll("SomeRandomText%#x0000;MoreRandomText%#x000D;NEWline" + 
	      "%%SomeRandomText%NUL;SomeMoreText%BEL;AndMoreText%%%LF;YayText!" + 
	      "SomeRandomText%#9;MoreRandomText%#13;NEWline%#rFF;"))
	  
	  println(e.replaceAll("SomeRandomText%#x0000;MoreRandomText%#x000D;NEWline" + 
	      "%%SomeRandomText%NUL;SomeMoreText%BEL;AndMoreText%%%LF;YayText!" + 
	      "SomeRandomText%#9;MoreRandomText%#13;NEWline%#rFF;", true))
	}
	
	def testJavaRegexFunctionality(){
	  val p: Pattern = Pattern.compile("([0-9]{3})|([0-9]{5})")
	  val m: Matcher = p.matcher("123456789000")
	  while(m.find()){
	    println(m.group())
	  }
	}
}