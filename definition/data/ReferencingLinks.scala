/**
 * Author: Peter Started:04.08.2010
 */
package definition.data
import java.io._


/** Stores the external links to this instance
 * @param myRef Reference of this instance
 * @param links Map that contains for each field all the external links to it
 * key = fildNr, value = list to references to external instances
 */
class ReferencingLinks(val ref:Reference,links:Map[Int,List[Reference]] =Map())
  extends Referencable{
  
	/** writes the Links info to IO
   *  
   * @param file where to write
   */
	override def write(file:DataOutput)=    {
  	 file.writeInt(links.size) 
  	 for((fnum,llist) <-links)
  	 {
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
		val newList=links(fieldNr).filter(s => s != target)
		// create a new object whith a new map that contains the new list
		new ReferencingLinks(ref, links(fieldNr)=newList )
	}
	
	/** adds a target link pointing to this instance
	 * 
	 * @param target the external instance pointing to this instance
	 * @param fieldNr fieldNr the number of the field in this instance it is pointing at
	 * @return a new ReferencingLinks object with the new settings
	 */
	def addTargetLink(target:Reference,fieldNr:Int ):ReferencingLinks = {
		// get the list for field "fieldNr" and adds the target link
		val newList=target ::links(fieldNr)
		// create a new object whith a new map that contains the new list
		new ReferencingLinks(ref, links(fieldNr)=newList )
	} 
}

object ReferencingLinks {
	
	/** read from io
	 * 
	 * @param file from where to read
	 */
	def read(ref:Reference,file:DataInput) = {
		val numFields=file.readInt
		var theMap=Map[Int,List[Reference]]()
		for(field <-0 until numFields) {
			val fieldNr=file.readInt // here could be a check for plausible values
			val listSize=file.readInt
			var theList:List[Reference]=Nil
			for(r <- 0 until listSize) {
				theList=Reference(file) :: theList
			}
			theMap=theMap + (fieldNr -> theList)
		}
		new ReferencingLinks(ref,theMap)
	}
}











