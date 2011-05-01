/**
 * Author: Peter Started:07.03.2011
 */
package client.dataviewer.sidePanel

/** Manages the List of SidePanelControllers
 * 
 */

import scala.swing._
import scala.collection.mutable.HashSet
import definition.typ.SystemSettings
import definition.data.Reference
import definition.typ.AbstractObjectClass

object SPControllerList {  
	println("PanelSetting:" +SystemSettings().getClientSetting("SidePanelControllers"))
  
	private lazy val classList:Seq[Class[_]] = try {
		val settingsString=SystemSettings().getClientSetting("SidePanelControllers").trim
		if (settingsString.length==0) Seq.empty 
		else settingsString.split(',').map(a=>{
			println("SidePanelController: '"+a.trim()+"'");
			Class.forName(a.trim())} )
	} catch {
		case e => System.err.println("Error when loading SidePanelController: "+e.toString);Seq.empty
	}
	
  
  
  private lazy val contrList=classList.map(_.newInstance.asInstanceOf[SidePanelController])
  
  
  def generateList(tableClass:AbstractObjectClass):Seq[SidePanelController]={
  	//println("generateList :"+tableClass.name)
  	contrList.filter(_.classFits(tableClass)).map(_.getClass.newInstance.asInstanceOf[SidePanelController])
  }
  
  
   
}