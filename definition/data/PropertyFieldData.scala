/**
 * Author: Peter Started:28.07.2010
 */
package definition.data

import java.io._
import scala.collection.immutable.IndexedSeq
/**
 * 
 */
class PropertyFieldData (val isSingle:Boolean,val propertyList: IndexedSeq[Reference]) {


	/** adds a child reference to the property list
	 * 
	 * @param ref the new child to add
	 * @param atPos the position in the list where the child should be added. -1=> at the end.
	 * The Position counts for instances of the same type
	 * @return a new instance of this propertyFieldDate with the new List
	 */
	def addPropertyInstance(ref:Reference,atPos:Int):PropertyFieldData = {
		if(isSingle && propertyList.length==1) 
			throw new IllegalArgumentException("Cant add more than 1 Property to a single property data")
		//System.out.println("addPropertyInstance "+ref+" "+atPos+ " oldList:"+propertyList)
		val newList= if(atPos< 0|| atPos>=propertyList.size) propertyList :+ ref
		   else  if(atPos==0) ref +: propertyList 
		   else {
		  	 val posOfNth=findPosOfNthOfType(propertyList,ref.typ,atPos)
		  	 //System.out.println("addPropertyInstance "+ref+" atPos: "+atPos+ " oldList:"+propertyList)
		  	 //System.out.println("pos of Nth:"+posOfNth)
		  	 if(posOfNth<0) propertyList :+ ref
		  	 else (propertyList.take(posOfNth):+ ref) ++ propertyList.drop(posOfNth)
		   }
		 //System.out.println("new List: "+newList)
		new PropertyFieldData(isSingle,newList )
		
	}
	
	private def findPosOfNthOfType(list:Seq[Reference],searchType:Int,numToFind:Int):Int = {
		var numFound:Int=0
		for(i <-list.indices;elem=list(i))
			if(elem.typ==searchType) {
				numFound +=1
				if(numFound==numToFind) return i+1				
			}
		return -1
	}

	def removePropertyInstance(ref:Reference) = {
    val newList=if(propertyList.length==1 && propertyList.head==ref) IndexedSeq.empty
    else propertyList.filter(x => x!=ref)
		new PropertyFieldData(isSingle,newList)
	}
	
	def moveInstanceToPos(ref:Reference,toPos:Int):PropertyFieldData = {
		if(isSingle && propertyList.length==1) 
			throw new IllegalArgumentException("Cant move more than 1 Property to a single property data")
		val oldPos=propertyList.indexOf(ref)
		if(oldPos<0 ) throw new IllegalArgumentException("Move property can find "+ref)
		val insertPos=if(oldPos<toPos)(toPos -1) else toPos
		val redList=propertyList.filterNot(_ == ref)
		val newList= if(toPos< 0|| toPos>=propertyList.size) redList :+ ref
		   else  if(toPos==0) ref +: redList 
		   else {
		  	 val posOfNth=findPosOfNthOfType(redList,ref.typ,toPos)
		  	 //System.out.println("addPropertyInstance "+ref+" atPos: "+toPos+ " oldList:"+propertyList)
		  	 //System.out.println("pos of Nth:"+posOfNth)
		  	 if(posOfNth<0) redList :+ ref
		  	 else (redList.take(posOfNth):+ ref) ++ redList.drop(posOfNth)
		   }
		 //System.out.println("new List: "+newList)
		new PropertyFieldData(isSingle,newList )
	}

	/*
	def clearAllProperty() = {
		for(ref <- propertyList) 
			master.pman.removeProperty(master.instRef,ref)
		new PropertyData(master,isSingle,Nil)}	
	 */
	def write(file:DataOutput) =   {
		file.writeBoolean(isSingle)
		file.writeShort(propertyList.size)
		for(p <-propertyList) p.write(file)
	}
	
	def dataLength:Int = {
		3 +propertyList.size*12
	}
		
		
	override def toString() = {
		"["+propertyList.foldLeft(" ")((r,c)=>r+c.sToString+", ")+"]"
	}



}

object PropertyFieldData
{
	def apply(file:DataInput) = {
		val single=file.readBoolean
		val count=file.readShort
		var plist:IndexedSeq[Reference]=IndexedSeq.empty
		for (i <-0 until count)
			plist= plist :+ Reference(file)
			new PropertyFieldData(single,plist)
	}

}