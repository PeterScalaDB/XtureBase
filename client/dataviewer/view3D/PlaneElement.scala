/**
 * Author: Peter Started:22.05.2011
 */
package client.dataviewer.view3D

import definition.data.{Reference,Referencable,InstanceData}
import definition.expression.{VectorConstant,Plane3D,Expression,NULLVECTOR}
import client.comm.SubscriptionFactory
import definition.typ.AllClasses
import java.io._

/**
 * 
 */
case class PlaneElement(ref:Reference,name:String,npos:VectorConstant,ndir:VectorConstant,color:Int) extends
  Plane3D(npos,ndir) with Referencable {	

}

object PlaneFactory extends SubscriptionFactory[PlaneElement] {
	
	registerClass(AllClasses.get.getClassIDByName("Plane"),createPlane)
	
	def emptyFunc(ref:Reference)= new PlaneElement(ref,"",NULLVECTOR,NULLVECTOR,0)
	
	def createPlane (ref:Reference,in:DataInput) = {		
		val fields=loadFields(in,4)	
		println("create Plane "+ref+" "+fields.mkString(","))
		new PlaneElement(ref,fields(0).getValue.toString,fields(1).getValue.toVector,
			fields(2).getValue.toVector,fields(3).getValue.toInt)
	}
}

