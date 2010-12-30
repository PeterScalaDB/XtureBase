/**
 * Author: Peter Started:27.12.2010
 */
package definition.data

import java.io.{DataInput,DataOutput}

/**
 * 
 */
case class PageData(pageNr:Int,elementList:Seq[PrintElement]) {
	
  def write(out:DataOutput) = {
  	out.writeInt(pageNr)
  	out.writeInt(elementList.size)
  	elementList.foreach(_.write(out))
  }
  
  override def toString = "Page "+pageNr+":\n"+elementList.mkString("\n")
}

object PageData {
	def apply(in:DataInput):PageData= {
		val pageNr=in.readInt
		val size=in.readInt
	  PageData(pageNr,for(i <- 0 until size) yield PrintElement(in))	
	}
}