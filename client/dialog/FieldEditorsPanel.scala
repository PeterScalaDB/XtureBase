/**
 * Author: Peter Started:10.10.2010
 */
package client.dialog

import scala.swing._
import definition.data._
import definition.typ._
import javax.swing.{BorderFactory}
import javax.swing.border._


/** panel that manages Field Editors for selected Instances
 * 
 */
class FieldEditorsPanel extends BorderPanel with SelectListener {
	
	//var instList:Seq[Referencable] = Seq.empty
	var groupList:Seq[SelectGroup[_<:Referencable]]= _
	var commonTyp:Int = -1
	var currentEditors=collection.mutable.ArrayBuffer[FieldEditor]()
	
	var vglue=Swing.VGlue
	
	val label=new Label("Felder:")
	label.preferredSize=new Dimension(50,35)
	
	val childPanel= new BoxPanel(scala.swing.Orientation.Vertical)
	childPanel.border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)
	
	//preferredSize=new Dimension(80,100)
	
	add (label,BorderPanel.Position.North)
	add (childPanel,BorderPanel.Position.Center)
	
	def selectionChanged [T <: Referencable](sender:SelectSender,groups:Seq[SelectGroup[T]]) = {
		groupList=groups.asInstanceOf[Seq[SelectGroup[_<:Referencable]]]
		val newCommonTyp=AllClasses.get.getCommonClassForGroups(groupList)
		//System.out.println("new common Typ:"+newCommonTyp)
		if(newCommonTyp!=commonTyp) { // type was changed
			childPanel.contents.clear
			currentEditors.clear
			if(commonTyp!= -1) { // there where editors				
			}
			commonTyp=newCommonTyp
			
			if(newCommonTyp> 0) {
				//System.out.println("fieldEditorPanel sel changed "+groups+" "+sender+" commonTyp:"+newCommonTyp)
				val editorNames=AllClasses.get.getClassByID(newCommonTyp).fieldEditors
				for(aName <-editorNames){
					val editor=EditorFactory.getEditor(aName)
					//System.out.println("editor:"+editor)
					childPanel.contents+=editor.getPanel
					currentEditors+=editor					
				}					
			}
			revalidate
			repaint
		}
		
	  notifyEditors	
	}
	
	def notifyEditors:Unit = {
		for(editor <-currentEditors) {
			editor.setData(groupList)
		}
	}
	
}




object EditorFactory {
	val editorCache=collection.mutable.HashMap[String,FieldEditor]()
	
	def getEditor(name:String):FieldEditor = {
		if(editorCache.contains(name)) editorCache(name)
		else {
			val newEditor=Class.forName(name).newInstance.asInstanceOf[FieldEditor]
			editorCache(name)=newEditor
			newEditor
		}
	}
}