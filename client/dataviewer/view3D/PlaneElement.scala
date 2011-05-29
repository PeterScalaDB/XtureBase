/**
 * Author: Peter Started:22.05.2011
 */
package client.dataviewer.view3D

import definition.data.{Reference,Referencable,InstanceData}
import definition.expression.{VectorConstant,Plane3D,Expression,NULLVECTOR}
import client.comm.SubscriptionFactory
import definition.typ.AllClasses
import java.io._
import definition.data.OwnerReference

/**
 * 
 */


case class PlaneElement(ref:Reference,owners:Array[OwnerReference],suOwners:Seq[OwnerReference],name:String,
	npos:VectorConstant,ndir:VectorConstant,color:Int) extends
  Plane3D(npos,ndir) with Referencable {
 
	//import PlaneElement._
	
	/*override type BuilderType=PlaneElement
		
	

	def createOffsetPlane(towards:VectorConstant,offset:Double):PlaneElement = {
		val orth=orthogonalThrough(towards).unit * offset
		createClone(pos+orth,dir)
	}*/
	//override def builder: PlaneBuilder[PlaneElement]= ElementBuilder
	
	override def createClone(newPos:VectorConstant,newDir:VectorConstant):PlaneElement= 
			new PlaneElement(ref,owners,suOwners,name,newPos,newDir,color)
	
	/** creates a new plane that is parallel to this plane, with a distance of offset toward the given point
	 * 
	 * @param towards in what direction
	 * @param offset offset measure, distance to the new plae
	 */
	def createOffsetPlane(towards:VectorConstant,offset:Double):PlaneElement = {
		val orth=orthogonalThrough(towards).unit * offset
		createClone(pos+orth,dir)
	}
	
}

object PlaneFactory extends SubscriptionFactory[PlaneElement] {
	
	registerClass(AllClasses.get.getClassIDByName("Plane"),createPlane)
	
	def emptyFunc(ref:Reference)= new PlaneElement(ref,Array(),Seq(),"",NULLVECTOR,NULLVECTOR,0)
	
	def createPlane (ref:Reference,in:DataInput) = {		
		val (owners,suOwners,fields)=loadFields(in,4)	
		//println("create Plane "+ref+" "+fields.mkString(","))
		new PlaneElement(ref,owners,suOwners,fields(0).getValue.toString,fields(1).getValue.toVector,
			fields(2).getValue.toVector,fields(3).getValue.toInt)
	}
	
	def apply(data:InstanceData)= {
		new PlaneElement(data.ref,data.owners,data.secondUseOwners,data.fieldValue(0).toString,data.fieldValue(1).toVector,data.fieldValue(2).toVector,
			data.fieldValue(3).toInt)
	}
	
	
}

/*object PlaneElement {
	implicit val builder=new PlaneBuilder[PlaneElement]{
		def createClone(oldv:PlaneElement,newPos:VectorConstant,newDir:VectorConstant)= 
			new PlaneElement(oldv.ref,oldv.owners,oldv.suOwners,oldv.name,newPos,newDir,oldv.color)
	}
}*/

