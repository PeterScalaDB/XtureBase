/**
 * Author: Peter Started:18.07.2010
 */
package definition.test

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import definition.typ._
import definition.expression._
import org.junit.Test
import org.junit.Before
import server.config._
import transaction.parser._

/** Tests constants and constant conversion
 * 
 */
class ConstantTest extends JUnitSuite 
{
  val intc=new IntConstant(199)
  val floatc=new DoubleConstant(1774.9901)
  val stringc=new StringConstant( "hello")
  
  val stringn=new StringConstant("934.003")
  
  val testExpr= List (
		"2 +4", "2+4.1", " 3.1*9","(1-2 )",".2+6.5+18","2.1*5*7","4+2*7.1","(4+2)*7","2*4+7","(2.*4)+3*5","2*(4+5)-1"	
	)
  
  
  @Test def convertTest()=
  {
  	val stringf=floatc.convertTo(DataType.StringTyp)
  	assertEquals(stringf.convertTo(DataType.DoubleTyp),floatc)
  	val floatf=stringn.convertTo(DataType.DoubleTyp )
  	assertEquals(floatf.convertTo(DataType.StringTyp),stringn)
  	
  	assertEquals(intc == stringc,false)
  	
  	println(stringf+ " "+floatf+" "+intc.toString)
  }
  
  @Test def binTest()=
  {
  	val a=new BinaryOperation(intc,BinOperator.getOp('*'),floatc);
  	//println(a+" "+a.getValue())
  	assertEquals(a.getValue,new IntConstant(199*1775))
  	val b=new BinaryOperation(floatc,BinOperator.getOp('+'),intc);
  	//println(b+" "+b.getValue())
  	assertEquals(b.getValue,new DoubleConstant(1774.9901+199.0))
  	val c=new BinaryOperation(intc,BinOperator.getOp('*'),b)
  	//println(c.getTerm)
  	val d=new BinaryOperation(a,BinOperator.getOp('+'),intc)
  	//println(d.getTerm)
  }
  
  
	
	@Test def parseTest()=
  {
		for (i <- testExpr)
		{
			val expr= StringParser.parse(i)
			println( expr+" = "+expr.getTerm+" => "+expr.getValue)
		}	 
		
		assertEquals( StringParser.parse(testExpr.head).getValue,new IntConstant(6) )
  }
}