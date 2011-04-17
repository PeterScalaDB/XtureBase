/**
 * Author: Peter Started:04.08.2010
 */
package definition.data
import java.io._

/**  reference to exernal target field
 * @param typ type of target instance
 * @param inst instance of target instance
 * @param field # of the field where the external fieldReference to this instance is located
 * @param isParentRef is it a parentRef or a fieldRef
 * 
 */
case class ExtFieldRef(typ:Int,inst:Int,field:Byte,isParentRef:Boolean){
	def write(file:DataOutput) = {
		//System.out.println("Write  "+this)
		file.writeInt(typ);file.writeInt(inst);file.writeByte(field);file.writeBoolean(isParentRef)		
	}
	def getReference=new Reference(typ,inst)
}

object ExtFieldRef {
	def read(file:DataInput) = {
		val ret=new ExtFieldRef(file.readInt,file.readInt,file.readByte,file.readBoolean)
		//System.out.println(ret)
		ret
	}
}


/** Stores the external links to this instance
 * @param ref Reference of this instance
 * @param links Map that contains for each field all the external links to it
 * key = fieldNr, value = list to references from external instances
 */
class ReferencingLinks(override val ref:Reference,val links:Map[Int,List[ExtFieldRef]] =Map())
  extends Referencable{
    
	/** writes the Links info to IO
   *  
   * @param file where to write
   */
	override def write(file:DataOutput)=    {
		 //print("write "+toString+" "+links.size)
  	 file.writeInt(links.size) 
  	 for((fnum,llist) <-links)
  	 {
  		 //print(" "+fnum+" -> "+llist.size)
  		 file.writeInt(fnum)
  		 file.writeInt(llist.size)
  		 for(ref <-llist)
  			 ref.write(file)
  	 }
   }
	
	override def toString = {
		"RefLink "+ref+" "+links
	}
	
	/** removes a target link pointing to this instance
	 * 
	 * @param target the external instance pointing to this instance
	 * @param fieldNr the number of the field in this instance it is pointing at
	 * @return a new ReferencingLinks object with the new settings
	 */
	def removeTargetLink(target:Reference,fieldNr:Int ):ReferencingLinks = {
		// get the list for field "fieldNr" and remove the target link
		val newList=links(fieldNr).filter(s => s.typ != target.typ || s.inst != target.instance )
		// create a new object whith a new map that contains the new list
		val newMap:Map[Int,List[ExtFieldRef]] = links.updated(fieldNr,newList)
		//System.out.println("Removing target Link target:"+target+ " field: "+fieldNr+ " Map:"+newMap)
		new ReferencingLinks(ref,newMap  )
	}
	
	/** adds a target link pointing to this instance
	 * 
	 * @param target the external instance pointing to this instance
	 * @param fieldNr fieldNr the number of the field in this instance it is pointing at
	 * @return a new ReferencingLinks object with the new settings
	 */
	def addTargetLink(target:ExtFieldRef,fieldNr:Int ):ReferencingLinks = {
		// get the list for field "fieldNr" and adds the target link
		val newList=target :: (if(links.contains(fieldNr))links(fieldNr) else Nil) 
		// create a new object whith a new map that contains the new list
		new ReferencingLinks(ref, links.updated(fieldNr,newList) )
	} 
}

object ReferencingLinks {
	
	/** read from io
	 * 
	 * @param file from where to read
	 */
	def read(ref:Reference,file:DataInput) = {
		val numFields=file.readInt
		var theMap=Map[Int,List[ExtFieldRef]]()
		for(field <-0 until numFields) {
			val fieldNr=file.readInt // here could be a check for plausible values
			val listSize=file.readInt
			var theList:List[ExtFieldRef]=Nil
			for(r <- 0 until listSize) {
				theList=ExtFieldRef.read(file) :: theList
			}
			theMap=theMap + (fieldNr -> theList)
		}
		new ReferencingLinks(ref,theMap)
	}
}











