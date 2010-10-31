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
import server.comm._

/**
 * 
 */
class StorageTests extends JUnitSuite 
{

	@Before def init() =	{
		val sc=new ServerClassList(xml.XML.loadFile(FSPaths.configDir+"types.xml"))
    AllClasses.set(sc)
		
		StorageManager.init(sc.classList)
		CommonSubscriptionHandler.init(AllClasses.get.getClassList.toMap)
	}

	@Test def storageManTest() =  {  	
		assert(AllClasses.get.getClassList.size>0)
		println(AllClasses.get.getClassList.toMap)
	}

	@Test def createTest() =  {  	
		val inst=StorageManager.createInstance(3,Array())
		println(inst)  	
		var newInst=inst.setField(0,new StringConstant("Object "+inst.ref.instance )).
		setField(1,new DoubleConstant(math.random*100 ))
		StorageManager.writeInstance(newInst,true)
		println(newInst)
		assert(newInst!=null)
	}

	@Test def createPropTest():Unit =   {
		assert(TransactionManager.doTransaction(0,{
			val inst=TransactionManager.tryCreateInstance(3,Array(new OwnerReference(0,new Reference(3,1))),true)
			println(inst)			
			assert (TransactionManager.tryWriteInstanceField(inst.ref,0,new StringConstant("Sub-Object "+inst.ref.instance )))
			assert (TransactionManager.tryWriteInstanceField(inst.ref,1,new DoubleConstant(math.random*200 )))			
		})==None)
	}
	
	@Test def movePropTest():Unit = {
		val oref=new OwnerReference(0,new Reference(3,1))
		var inst:InstanceData=null
		
		assert(TransactionManager.doTransaction(0, {			
			inst=TransactionManager.tryCreateInstance(3,Array(),true)			
		  assert (TransactionManager.tryWriteInstanceField(inst.ref,0,
				new StringConstant("Copytest-Object "+inst.ref.instance+" "+math.round(math.random*1000) )))			
		})==None)
		
		StorageManager.getInstanceProperties(Reference(3,1)) match {
			case Some(prop) => {
		     for(p <-prop.propertyFields(0).propertyList)
		    	 assert( TransactionManager.doTransaction(0,{
		    		 TransactionManager.tryCopyInstance(p,oref,new OwnerReference(0,inst.ref),true)
		    	 })==None)
		    	 
			}
			case _ => println("No properties found ")
		}		
	}
	
	@Test def linkTest():Unit = {
		assert(TransactionManager.doTransaction(0,{
			assert(TransactionManager.tryWriteInstanceField(new Reference(3,3),1,new FieldReference(None,Some(2),1)))
			assert(TransactionManager.tryWriteInstanceField(new Reference(3,4),0,
				new BinaryOperation(new DoubleConstant(4),BinOperator.getOp('+'), new FieldReference(None,Some(2),1))))
			assert(TransactionManager.tryWriteInstanceField(new Reference(3,4),1,
				new BinaryOperation(new DoubleConstant(19),BinOperator.getOp('-'), new FieldReference(Some(3),Some(3),1))))
		})==None)
		assert(TransactionManager.doTransaction(0,{
			val inst=TransactionManager.tryCreateInstance(3,Array(new OwnerReference(0,new Reference(3,1))),true)
		
			TransactionManager.tryCopyInstance(Reference(3,4),new OwnerReference(0,Reference(3,3)),OwnerReference(0,inst.ref),true)
		})==None)
	}

	
	
	@Test def collFuncTest() = {
		assert(TransactionManager.doTransaction(0, {
			assert(TransactionManager.tryWriteInstanceField(new Reference(3,1),1,new CollectingFuncCall("doubleSum",0,3,1)))
		})==None)
		assert(TransactionManager.doTransaction(0, {
			assert(TransactionManager.tryWriteInstanceField(new Reference(3,5),1,new IntConstant(50)))
		})==None	)		
	}
	
	@Test def readTest() =  {
		val maxID=StorageManager.getHandler(3).lastID 

		for(i <-1 to maxID.toInt)
			if(StorageManager.instanceExists(3,i))
				println(StorageManager.getInstanceData(new Reference(3,i)))
		StorageManager.shutDown()		
	}






}