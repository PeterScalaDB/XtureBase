/**
 * Author: Peter Started:25.07.2010
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
class StorageTests extends JUnitSuite 
{

	@Before def init() =	{		

		AllClasses.fromXML( xml.XML.loadFile(FSPaths.configDir+"types.xml"))
		StorageManager.init(AllClasses.getClassList)

	}

	@Test def storageManTest() =  {  	
		assert(AllClasses.getClassList.size>0)
		println(AllClasses.getClassList)
	}

	@Test def createTest() =  {  	
		val inst=StorageManager.createInstance(3,Array())
		println(inst)  	
		var newInst=inst.setField(0,new StringConstant("Object "+inst.ref.instance )).
		setField(1,new DoubleConstant(Math.random*100 ))
		StorageManager.writeInstance(newInst)
		println(newInst)
		assert(newInst!=null)
	}

	@Test def createPropTest():Unit =   {
		assert(TransactionManager.doTransaction{
			val inst=TransactionManager.tryCreateInstance(3,Array(new OwnerReference(0,new Reference(3,1))))
			println(inst)			
			assert (TransactionManager.tryWriteInstanceField(inst.ref,0,new StringConstant("Sub-Object "+inst.ref.instance )))
			assert (TransactionManager.tryWriteInstanceField(inst.ref,1,new DoubleConstant(Math.random*200 )))			
		}==true)
	}
	
	@Test def movePropTest():Unit = {
		val oref=new OwnerReference(0,new Reference(3,1))
		var inst:InstanceData=null
		
		assert(TransactionManager.doTransaction {			
			inst=TransactionManager.tryCreateInstance(3,Array())			
		  assert (TransactionManager.tryWriteInstanceField(inst.ref,0,
				new StringConstant("Copytest-Object "+inst.ref.instance+" "+Math.round(Math.random*1000) )))			
		}==true)
		
		StorageManager.getInstanceProperties(Reference(3,1)) match {
			case Some(prop) => {
		     for(p <-prop.propertyFields(0).propertyList)
		    	 assert( TransactionManager.doTransaction{
		    		 TransactionManager.tryCopyInstance(p,oref,new OwnerReference(0,inst.ref))
		    	 }==true)
		    	 
			}
			case _ => println("No properties found ")
		}		
	}
	
	@Test def linkTest():Unit = {
		assert(TransactionManager.doTransaction{
			assert(TransactionManager.tryWriteInstanceField(new Reference(3,3),1,new FieldReference(None,Some(2),1)))
			assert(TransactionManager.tryWriteInstanceField(new Reference(3,4),0,
				new BinaryOperation(new DoubleConstant(4),BinOperator.getOp('+'), new FieldReference(None,Some(2),1))))
			assert(TransactionManager.tryWriteInstanceField(new Reference(3,4),1,
				new BinaryOperation(new DoubleConstant(19),BinOperator.getOp('-'), new FieldReference(Some(3),Some(3),1))))	
			val inst=TransactionManager.tryCreateInstance(3,Array(new OwnerReference(0,new Reference(3,1))))
			TransactionManager.tryCopyInstance(Reference(3,4),new OwnerReference(0,Reference(3,3)),OwnerReference(0,inst.ref))
		})
	}

	@Test def readTest() =  {

		val maxID=StorageManager.getHandler(3).lastID 

		for(i <-1 to maxID.toInt)
			if(StorageManager.instanceExists(3,i))
				println(StorageManager.getInstanceData(new Reference(3,i)))
		StorageManager.shutDown()
	}






}