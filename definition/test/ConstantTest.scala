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
  val fr=new FieldReference(None,None,0)
  val fc=new FunctionCall(None,"sum",List(intc))
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
  	val d=new BinaryOperation(a,BinOperator.getOp('+'),new BinaryOperation(intc,BinOperator.getOp('-'),fc))
  	val f=new BinaryOperation(d,BinOperator.getOp('*'),fr)
  	val g=new FunctionCall(None,"Sum",List(f,floatc))
  	
  	println(g.getTerm)
  	val dlist=g.getElementList[DoubleConstant](DataType.IntTyp,List[DoubleConstant](floatc))
  	println(dlist)
  	
  	val rlist=g.getElementList[FieldReference](DataType.FieldRefTyp,List[FieldReference]())
  	println(rlist)
  	println("CacheValue: "+rlist.head.cachedValue)
  	
  	val blist=g.getElementList[FunctionCall](DataType.FunctionCall,List[FunctionCall]())
  	println(blist)
  	println("CacheValue: "+blist.head.cacheValue)
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