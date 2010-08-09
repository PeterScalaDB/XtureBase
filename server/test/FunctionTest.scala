/**
 * Author: Peter Started:07.08.2010
 */
package server.test
import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import definition.typ._
import definition.expression._
import org.junit._
import server.config._
import server.storage._
import definition.data._
import transaction.handling._

/**
 * 
 */
class FunctionTest  extends JUnitSuite  {
	
	@Before def init() =	{	
		AllClasses.fromXML( xml.XML.loadFile(FSPaths.configDir+"types.xml"))
		StorageManager.init(AllClasses.getClassList)
	}
	
	@Test def funcTest() = {
		assert(FunctionManager.get.getFunctionValue(None,"max",List(DoubleConstant(144),DoubleConstant(567)))==DoubleConstant(567))
		println(FunctionManager.get.getFunctionValue(None,"sin",List(DoubleConstant(0.5) )) )
		println(new FunctionCall(None,"sin",List(DoubleConstant(math.Pi/2))).getValue)
	}
	
	
	
  
}