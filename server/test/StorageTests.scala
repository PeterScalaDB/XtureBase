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
			var newInst=inst.setField(0,new StringConstant("Sub-Object "+inst.ref.instance )).
			setField(1,new DoubleConstant(Math.random*200 ))		
			println(newInst)
			assert(newInst!=null)
			TransactionManager.tryWriteInstanceData(newInst)
		}==true)
	}
	
	@Test def movePropTest():Unit = {
		val oref=new OwnerReference(0,new Reference(3,1))
		var inst:InstanceData=null
		
		assert(TransactionManager.doTransaction {			
			inst=TransactionManager.tryCreateInstance(3,Array())
			inst=inst.setField(0,new StringConstant("Copytest-Object "+inst.ref.instance+" "+Math.round(Math.random*1000) ))
			   TransactionManager.tryWriteInstanceData(inst)
			
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

	@Test def readTest() =  {

		val maxID=StorageManager.getHandler(3).lastID 

		for(i <-1 to maxID.toInt)
			if(StorageManager.instanceExists(3,i))
				println(StorageManager.getInstanceData(new Reference(3,i)))
	}






}