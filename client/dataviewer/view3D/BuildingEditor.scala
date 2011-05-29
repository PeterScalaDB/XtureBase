/**
 * Author: Peter Started:22.05.2011
 */
package client.dataviewer.view3D

import scala.swing._
import definition.typ.{CustomInstanceEditor,SelectGroup}
import definition.data.{Reference,Referencable}
import java.awt.{Dimension,Color}
import java.awt.event.{MouseAdapter,MouseEvent,InputEvent,FocusListener,FocusEvent}
import client.comm.ClientQueryManager
import client.dialog.{SelectSender,SelectListener,ContainerFocusListener}
import javax.media.j3d._
import com.sun.j3d.utils._
import com.sun.j3d.utils.universe._
import javax.vecmath.{Point3d,Vector3d,Color3f}
import com.sun.j3d.utils.pickfast._
import client.dialog.SelectEventDispatcher
import client.dialog.NewPanelArea
import java.awt.BorderLayout
import definition.expression.VectorConstant
import client.dialog.PointAnswerPanel
import client.dialog.ReferenceAnswerPanel

/**
 * 
 */



class BuildingEditor extends BorderPanel with CustomInstanceEditor  {
  
	
	preferredSize=new Dimension(100,400)
	minimumSize=preferredSize	
	
	val textEdit=new TextArea
	val config =SimpleUniverse.getPreferredConfiguration()	
	val controller=new BEViewController(this)
	//var cursorVisible:Boolean=false	
	
	val  canvas3D = new Canvas3D(config){
		setFocusable(true)
		override def postRender = {
			
			super.postRender
			for(c<-currentPrism) {
			val g2=this.getGraphics2D
			g2.setColor(Color.black)			
			g2.drawString(currentPrism .map(_.name).getOrElse("-")+" ",0,20)
			g2.drawString(controller.editorState.toString+(if(controller.bracketMode)" S-Modus" else " "),0,40)			
			if(isFocusOwner){
				g2.setColor(Color.blue)
				val s=getSize
				g2.drawRect(0,0,s.width-1,s.height-1)
			}
			g2.flush(true)
			}
		}
		addMouseListener(new MouseAdapter() {
			override def mousePressed(e:MouseEvent)= {
				requestFocusInWindow()
				controller.mousePressed(e,(e.getModifiersEx& InputEvent.BUTTON1_DOWN_MASK)>0,
				(e.getModifiersEx& InputEvent.BUTTON3_DOWN_MASK)>0)				
			}
		})		
		addFocusListener(new FocusListener() {
			def focusGained(e:FocusEvent)= {
				PointAnswerPanel.currentViewController=controller
				ReferenceAnswerPanel.currentViewController=controller
				//for(w<-workArea)notifyContainerListeners(w,"WorkArea")
				repaint
			}
			def focusLost(e:FocusEvent)= {
				repaint
			}
		})
	}
	
	val canvComp=new Component {
		override lazy val peer=new javax.swing.JPanel
		peer.setLayout(new java.awt.BorderLayout)
		peer.add(canvas3D,BorderLayout.CENTER)
		//preferredSize=new Dimension(150,100)
	}	
	
	
	val simpleU = new SimpleUniverse(canvas3D)	
	val superBranch=new BranchGroup
	
	val pickCanvas:PickCanvas=new PickCanvas(canvas3D,superBranch)
	pickCanvas.setMode(PickInfo.PICK_GEOMETRY)
	pickCanvas.setTolerance(3)
	
	val navBG=new BranchGroup
	
	val cursorBranchGroup=new BranchGroup
	cursorBranchGroup.setCapability(BranchGroup.ALLOW_DETACH)
	cursorBranchGroup.setCapabilityIsFrequent(0)
	val cursorTransG=new TransformGroup
	cursorTransG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
	cursorTransG.setCapabilityIsFrequent(0)
	val cursorTransform=new Transform3D
	cursorTransG.addChild(new CursorShape)
	cursorBranchGroup.addChild(cursorTransG)
	cursorBranchGroup.compile
	
	var currentPrism:Option[BorderPrism]=None
	
	val myMouseBeh=new MouseViewBehavior(viewPlatTrans)
	
  def getComponent(): Component = this
  
  var workArea:Option[WorkArea]=None

  def load(ref: Reference): Unit = {  
  	workArea=Some(new WorkArea(ref,this))
  	workArea.get.addPrismListener(workAreaLoaded)
  }

  def shutDown(): Unit = if(workArea.isDefined){  
  	workArea.get.shutDown()
  	workArea=None
  }
  
  def init()= {
  	textEdit.preferredSize=new Dimension(50,100)
  	val back=new Background(new Color3f(Color.white))
		back.setApplicationBounds(new BoundingSphere(new Point3d(0f,0f,0f),100))
    superBranch.addChild(back)
  	superBranch.setCapability(Group.ALLOW_CHILDREN_WRITE)
		superBranch.setCapability(Group.ALLOW_CHILDREN_EXTEND)
		superBranch.setCapabilityIsFrequent(0)
		simpleU.addBranchGraph(superBranch)
		simpleU.getViewer.getView.setBackClipDistance(100)
		val cameraTrans = new Transform3D();
		cameraTrans.lookAt(new Point3d(50f,-50f,50f),new Point3d(0,0,0),new Vector3d(0,0,1))
		cameraTrans.invert
			//cameraTrans.rotZ(0)
		viewPlatTrans.setTransform(cameraTrans)    
		
		myMouseBeh.setSchedulingBounds(new BoundingSphere(new Point3d(0,0,0),1000))
		navBG.addChild(myMouseBeh)
		viewPlatTrans.addChild(navBG)
  }
  
  def workAreaLoaded(data:Option[BorderPrism]):Unit = /*ClientQueryManager.runSw*/{
  	data match {
  		case Some(d)=>{
  			for(old<-currentPrism) superBranch.removeChild(old)
  			println("work loaded "+d+" "+d.numChildren+"\n chi:"+d.getChild(0))  			
  			textEdit.text=d.toString  			  			
  			d.compile()  			
  			superBranch.addChild(d)
  		}
  		case None=> {
  			textEdit.text="No prism"  			
  		}
  	}
  	currentPrism=data
  }

  def editorName(): String = "BuildingEditor"

  def viewPlatTrans =simpleU.getViewingPlatform().getViewPlatformTransform()
  
  //add(textEdit,BorderPanel.Position.West)
	add(canvComp,BorderPanel.Position.Center)
	init()
  
	//************************ Select	
	
	
  
  def shapeDeleted(shRef:Reference)= {
  	for(w<-workArea;group <-controller.currentSelection;if group.children.exists(_.ref ==shRef)) {
  		controller.deselect(true)
  	}  		
  }	
  
  def setCursorTo(pos:VectorConstant)={
  	hideCursor()
  	cursorTransform.setTranslation(new Vector3d(pos.toPoint))
  	cursorTransG.setTransform(cursorTransform)  	
  	superBranch.addChild(cursorBranchGroup)
  	canvas3D.repaint
  }
  
  def hideCursor()= if(controller.bracketMode){
  	superBranch.removeChild(cursorBranchGroup)  	
  }
  
  def hideNewElementBranch()= {
  	
  }
}

class CursorShape() extends Shape3D{
	 val offset=.5f
	 val points=Array(new Point3d(-offset,0,0),new Point3d(offset,0,0),
		                new Point3d(0,-offset,0),new Point3d(0,offset,0),
		                new Point3d(0,0,-offset),new Point3d(0,0,offset))
	 val pa=new LineArray(6,GeometryArray.COORDINATES)
	 pa.setCoordinates(0,points)
	 setGeometry(pa)
	 val app=new Appearance()
	 app.setColoringAttributes(new ColoringAttributes(new Color3f(1f,0.1f,0.1f),ColoringAttributes.NICEST))
	 setAppearance(app)
}

