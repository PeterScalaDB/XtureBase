/**
 * Author: Peter Started:26.12.2010
 */
package client.print

import javax.print._
import javax.print.attribute._
import javax.print.attribute.standard._
import definition.data.OutputDefinition
/**
 * 
 */
object MediaMap {
	val mmUnit=Size2DSyntax.MM	
	
  private val mediaHash=collection.mutable.HashMap[MediaSizeName,MediaSizeWrapper]()
  def getMediaSize(msn:MediaSizeName) = {
		val ret=if(mediaHash.contains(msn))mediaHash(msn)
  	else {
  		val newWrapper=new MediaSizeWrapper(msn)
  		mediaHash(msn)=newWrapper
  		newWrapper
  	}
		//println("Map "+msn+" ->"+ret+" "+ret.mn .getValue)
		ret
  }
}

case class MediaSizeWrapper(val mn:MediaSizeName){
	lazy val mediaSize=MediaSize.getMediaSizeForName(mn)
	lazy val name=cutString(mn.toString.replace("iso-"," ").replace("jis-"," ").trim.toUpperCase,15)
	lazy val width=mediaSize.getX(MediaMap.mmUnit)
	lazy val height=mediaSize.getY(MediaMap.mmUnit)
	
	override def toString=name+" ("+Math.round(width)+" x "+Math.round(height)+")"+mn.getValue
	
	def cutString(st:String,len:Int)= {
		if(st.length>len) st.substring(0,len)
		else st
	}
}



object TrayMap {	
	
	val altClass=Class.forName("sun.print.SunAlternateMedia")
	val altConst=altClass.getDeclaredConstructor(classOf[Media])
	
  private val trayHash=collection.mutable.HashMap[MediaTray,MediaTrayWrapper]()
  
  def getMediaTray(mt:MediaTray) = {
  	if(trayHash.contains(mt))trayHash(mt)
  	else {
  		val newWrapper=new MediaTrayWrapper(mt)
  		trayHash(mt)=newWrapper
  		newWrapper
  	}
  }
}


class MediaTrayWrapper(val mt:MediaTray){
	lazy val altValue:PrintRequestAttribute=TrayMap.altConst.newInstance(mt).asInstanceOf[PrintRequestAttribute]	
	
	private lazy val name = if(OutputDefinition.trayTranslations.contains(mt.toString))OutputDefinition.trayTranslations(mt.toString) else mt.toString
	
	override def toString= name+" "+mt.getValue
}

object AutoTray extends MediaTrayWrapper(null) {
	override def toString= "Automatische Auswahl"
	override lazy val altValue=null
}