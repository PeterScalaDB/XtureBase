/**
 * Author: Peter Started:27.05.2011
 */
package client.dataviewer.view3D

import client.dialog.{AbstractViewController,PointClickListener}
import definition.expression.VectorConstant
import client.dialog.{SelectSender,SelectListener,ContainerFocusListener}
import client.dialog.SelectEventDispatcher
import client.dialog.NewPanelArea
import definition.typ.SelectGroup
import definition.data.Referencable
import java.awt.event.MouseEvent
import javax.media.j3d.PickInfo
import collection.JavaConversions._
import javax.media.j3d.GeometryArray
import javax.vecmath.Point3d
import client.dialog.ObjectSelectListener
import definition.expression.ObjectReference

object EditorState extends Enumeration {
	val Select=Value("Select")
	val AskPoint=Value("AskPoint")
	val LineTo=Value("LineTo")
	val ChooseObject=Value("Choose Object")
}


/**
 * 
 */
class BEViewController(editor:BuildingEditor) extends AbstractViewController with SelectSender {

	var pointListener:PointClickListener=_
	var objSelectListener:Option[ObjectSelectListener]=None
	var objSelectClassConstraints:Seq[Int]=Seq.empty
	var lastSelectedPoint:VectorConstant=new VectorConstant(0,0,0)	
	var bracketMode:Boolean = false
	var editorState:EditorState.Value=EditorState.Select
	var rubberStartPoint:VectorConstant=null
	var fitDistance=0.1f
	
	def mousePressed(e:MouseEvent,leftBut:Boolean,rightBut:Boolean):Unit= {		
		if(leftBut){
			editorState match {
				case EditorState.Select => {
					editor.pickCanvas.setFlags(PickInfo.NODE)
					editor.pickCanvas.setShapeLocation(e)
					val pickData=editor.pickCanvas.pickAllSorted
					if(pickData==null) deselect(true)
					else {
						val shapes=pickData.map(_.getNode).collect({case s:RefShape=> s})
						if(!shapes.isEmpty)
							selectSingleShape(shapes.first)
					}
				}
				case EditorState.AskPoint => {
					editor.pickCanvas.setFlags(PickInfo.CLOSEST_GEOM_INFO)
					editor.pickCanvas.setShapeLocation(e)
					val ret=editor.pickCanvas.pickClosest
					if(ret!=null&&ret.getIntersectionInfos.size==1){
						val result=getIntersectionFromVertex(ret.getIntersectionInfos().apply(0))
						if(result._1) internSetPoint(new VectorConstant(result._2))								
					}					
				}
				case EditorState.ChooseObject => {
					editor.pickCanvas.setFlags(PickInfo.NODE)
					editor.pickCanvas.setShapeLocation(e)
					val pickData=editor.pickCanvas.pickAllSorted
					if(pickData!=null) {
						println("Pick nodes :"+pickData.map(_.getNode).mkString(","))
						val shapes=pickData.map(_.getNode).collect(
							{case s:RefShape if(objSelectClassConstraints.contains(s.ref .typ) )=> s})
						println("Pick shapes "+shapes.mkString(","))	
						if(!shapes.isEmpty&&objSelectListener.isDefined)
						  objSelectListener.get.objectsSelected(ObjectReference(shapes.first.ref))	
					} 
				}
			}					
		}
		else if(rightBut) {			
			editorState match { // switch bracket mode
				case EditorState.AskPoint| EditorState.LineTo => {
					if(bracketMode) stopBracketMode
					else startBracketMode
				}
				case _ => 
			}
					
		}
	}
	import javax.media.j3d.PickInfo._	
	
	
	def getIntersectionFromVertex(info:PickInfo#IntersectionInfo)= {
	  info.getGeometry match {
	  	case l:GeometryArray=>{
	  		val pointsArray=info.getVertexIndices.toSeq.map (ix=> {
	  			val point=new Point3d
	  			l.getCoordinate(ix, point)
	  			(point,point.distance(info.getIntersectionPoint))
	  		} )
	  		
	  		val nearestPoint=pointsArray.reduceLeft((x, y) => if (x._2<=y._2) x else y)
	  		if(nearestPoint._2<fitDistance)(true,nearestPoint._1)
	  		else (false,nearestPoint._1)
	  	}
	  	case o=> (false,info.getIntersectionPoint)
	  }
	}
	
	
  def startBracketMode(): Unit = {	
		editor.setCursorTo(lastSelectedPoint)
		bracketMode=true
	}

  def stopBracketMode(): Unit = {   	
  	editor.hideCursor()
  	bracketMode=false
  	processPoint(lastSelectedPoint)
  }
  
  def internSetPoint(point:VectorConstant) = {
  	lastSelectedPoint=point
  	if(bracketMode) editor.setCursorTo(point)
  	else processPoint(point)
  }
  
  def processPoint(point:VectorConstant)= {
  	editorState match {
			case EditorState.LineTo => {			
			}
			case _ =>
		}
  	rubberStartPoint=point
  	pointListener.pointClicked(point)
  }

  def addDelta(x: Double, y: Double, z: Double): Unit =  internSetPoint(lastSelectedPoint+(x,y,z))  

  def setCoordinate(x: Double, y: Double, z: Double): Unit = { internSetPoint(new VectorConstant(x,y,z)) }

  def askForPointClick(plistener: PointClickListener): Unit = {  
  	pointListener=plistener
  	changeViewportState(EditorState.AskPoint)
  }

  def askForLineTo(plistener: PointClickListener, constraints: String): Unit = {  
  	
  }
  
  def askForObjectSelection(listener:ObjectSelectListener,constraints:String)={
  	objSelectListener=Some(listener)
  	objSelectClassConstraints=constraints.trim.split(',').map(_.toInt)
  	println("ask for selection, types:"+objSelectClassConstraints.mkString(","))
  	changeViewportState(EditorState.ChooseObject )
  }

  def deselect(): Unit = deselect(false)

  def changeViewportState(newState:EditorState.Value) = {
  	println("Editor change State :"+newState)
		stopModus
		editorState=newState	
		editor.canvas3D.repaint
	}
  
  def cancelModus(): Unit = {  
  	editor.hideNewElementBranch()
  	changeViewportState(EditorState.Select)
  }
  
  def stopModus() = {
  	editor.hideCursor
  }
  
  def requestFocus(): Unit = {editor.canvas3D.requestFocusInWindow;editor.canvas3D.repaint  }
  
  val selectListeners=collection.mutable.HashSet[SelectListener]()
	val containerFocusListeners=collection.mutable.HashSet[ContainerFocusListener]()
	var currentSelection:Seq[SelectGroup[RefShape]]=Seq.empty
	
	registerSelectListener(SelectEventDispatcher)
  registerContainerFocusListener(NewPanelArea)
	
	def deselect(notify:Boolean)= {
  	for(w<-editor.workArea;group <-currentSelection;c<-group.children)
  		w.deselectShape(c)
  	currentSelection=Seq.empty
  	if(notify) notifySelectListeners
  	for(cfl <- containerFocusListeners;if(cfl.alsoDeselect))
			cfl.containerFocused(null,0)
		cancelModus
  }
  
  def selectSingleShape(shape:RefShape)= {
  	deselect(false)
  	for(w<-editor.workArea) {
  	  currentSelection=List(new SelectGroup(shape.ownerRef,Seq(shape)))
  	  w.selectShape(shape)
  	  notifySelectListeners
  	  shape match {
  	  	case v:VolumeShape=> notifyContainerListeners(v,"VolumeShape") 
  	  	case _=>
  	  }
  	}  	
  }
  
  def registerSelectListener(listener:SelectListener) = {
		selectListeners+=listener
	}
	
	def registerContainerFocusListener(newList:ContainerFocusListener) = 
		containerFocusListeners+=newList
		
	def notifyContainerListeners(superInst:Referencable,contName:String) = for (w<-editor.workArea){		
		for(list<-containerFocusListeners)
			list.containerFocused(superInst,0,contName)
	}
	
	def notifySelectListeners= for (l<-selectListeners) l.selectionChanged(this,currentSelection)
	
}