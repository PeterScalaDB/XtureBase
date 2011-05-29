/**
 * Author: Peter Started:18.05.2011
 */
package client.dataviewer.view3D
import swing._
import javax.media.j3d._
import com.sun.j3d.utils._
import com.sun.j3d.utils.universe._
import com.sun.j3d.utils.geometry.ColorCube
import javax.vecmath._
import java.awt.{Graphics,Color,Font}
import java.awt.event.{KeyEvent,MouseEvent,InputEvent,MouseWheelEvent}
import com.sun.j3d.loaders.ParsingErrorException
import com.sun.j3d.loaders.IncorrectFormatException
import com.sun.j3d.loaders.Scene
import scala.swing.event.ButtonClicked
import scala.collection.JavaConversions._
import com.sun.j3d.utils.geometry.Text2D
import com.sun.j3d.utils.behaviors.keyboard._
import com.sun.j3d.utils.behaviors.vp._
import com.sun.j3d.utils.behaviors.picking._
//import com.microcrowd.loader.java3d.max3ds.Loader3DS
//import org.web3d.vrml.j3d.loaders.VRML97Loader
//import com.sun.j3d.loaders.objectfile.ObjectFile?
//import org.jdesktop.j3d.loaders.vrml97.VrmlLoader


/**
 * 
 */
object v3DTest extends SimpleSwingApplication  {
	val config =SimpleUniverse.getPreferredConfiguration();
	var lastTime=0L
	var framesCount=0
	var framesPerSec=0d
	var timeDif=0L	
	
	
	val  canvas3D = new Canvas3D(config){
		//setDoubleBufferEnable(false)
		override def postRender = {
			framesCount+=1
			if(framesCount % 50 == 0) {
				val now=System.currentTimeMillis
				timeDif=now-lastTime
				lastTime=now
				framesPerSec=50*1000/timeDif
			}
			super.postRender
			val g2=this.getGraphics2D
			g2.setColor(Color.red)			
			//val b=getBounds
			//println("p")
			g2.drawString("50 Frames in "+timeDif+"ms = "+framesPerSec.toString+"F/s",0,20)
			g2.flush(true)
		}

	};

	val textArea=new TextArea	

	val canvComp=new Component {
		override lazy val peer=new javax.swing.JPanel
		peer.setLayout(new java.awt.BorderLayout)
		peer.add(canvas3D,"Center")		
	}
	val simpleU = new SimpleUniverse(canvas3D)
	val superBranch=new BranchGroup
	
	val p = new PickObject(canvas3D,superBranch)
	//var scene:Scene = null
	
	def viewPlatTrans =simpleU.getViewingPlatform().getViewPlatformTransform();
	val keyNavBeh = new KeyNavigatorBehavior(viewPlatTrans);
	val keyNavBG=new BranchGroup
	val myMouseBeh=new MouseViewBehavior(viewPlatTrans)

	val fileNameEdit=new TextField
	val loadBut=new Button("Laden")

	val loadPanel=new BoxPanel(Orientation.Horizontal){
		contents+=fileNameEdit+=Swing.HStrut(20)+=loadBut
		listenTo(loadBut)
		reactions+={
			case ButtonClicked(`loadBut`)=> loadFile(fileNameEdit.text)
		}
	}
	
	var currentScene:BranchGroup=null

	def loadFile(text:String):Unit = {
		superBranch.removeAllChildren()
		//if(currentScene!=null)currentScene.removeChild(keyNavBG)
		currentScene = createSceneGraph(text)	
		currentScene.setCapability(BranchGroup.ALLOW_DETACH)
		currentScene.setCapabilityIsFrequent(0)
		currentScene.compile()
		superBranch.addChild(currentScene)
		println("super:"+superBranch.getBounds)
	}


	def top = new MainFrame {
		title = "Hello, World!"
			preferredSize  = new Dimension(700,400)
			contents = new BorderPanel{
			add(canvComp,BorderPanel.Position.Center)
			add(loadPanel,BorderPanel.Position.South)			
			//simpleU.getViewingPlatform().setNominalViewingTransform();
			superBranch.setCapability(Group.ALLOW_CHILDREN_WRITE)
			superBranch.setCapability(Group.ALLOW_CHILDREN_EXTEND)
			superBranch.setCapabilityIsFrequent(0)
			simpleU.addBranchGraph(superBranch)
			val cameraTrans = new Transform3D();
			//cameraTrans.setTranslation()
			//val mat=new Matrix3d()
			cameraTrans.lookAt(new Point3d(2f,-2f,2f),new Point3d(0,0,0),new Vector3d(0,0,1))
			cameraTrans.invert
			//cameraTrans.rotZ(0)
			viewPlatTrans.setTransform(cameraTrans)
			
      keyNavBeh.setSchedulingBounds(new BoundingSphere(new Point3d(),1000.0));
			keyNavBG.addChild(keyNavBeh)
			myMouseBeh.setSchedulingBounds(new BoundingSphere())
			keyNavBG.addChild(myMouseBeh)
			viewPlatTrans.addChild(keyNavBG)		
			
			/*val orbitBeh=new OrbitBehavior()
			orbitBeh.setSchedulingBounds(new BoundingSphere())
			simpleU.getViewingPlatform().setViewPlatformBehavior(orbitBeh)*/
			loadFile("Ein Text in 3D")
		}
	} 

	

	def createSceneGraph(text:String)= {		
		val objRoot = new BranchGroup()	
		try {
			val back=new Background(.5f,.3f,.42f)
			back.setApplicationBounds(new BoundingSphere(new Point3d(0f,0f,0f),100))
      objRoot.addChild(back)
			val rotatex = new Transform3D();
			//rotatex.rotX(-Math.Pi/4.0d)   
			val objRotate = new TransformGroup(rotatex)
			objRotate.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
			objRotate.setCapabilityIsFrequent(0)
			//val rotationAlpha = new Alpha(-1, 8000)
			//val rotator = new RotationInterpolator(rotationAlpha, objRotate)
			val myBeh=new SimpleBehavior(objRotate)
			
			myBeh.setSchedulingBounds(new BoundingSphere())			
			objRotate.addChild(myBeh)
			
			val wGruppe=new BranchGroup()
			val trans=new Transform3D
			//trans.setTranslation(new Vector3d(0,.7,0))
			trans.setScale(.02)
			val transGroup=new TransformGroup(trans)
			val cube=new ColorCube(0.3)
			val cattrib=new Appearance()    
			val tattrib=new TransparencyAttributes(TransparencyAttributes.NICEST,0.5f)
			cattrib.setTransparencyAttributes(tattrib)
			cube.setAppearance(cattrib)
			val font3d = new Font3D(new Font("Helvetica", Font.PLAIN, 10),new FontExtrusion());
			val textGeom = new Text3D(font3d, text,new Point3f(0.0f, 0.0f, 0.0f),Text3D.ALIGN_CENTER,Text3D.PATH_RIGHT);
			val textShape = new Shape3D(textGeom)			
			transGroup.addChild(textShape)			
			wGruppe.addChild(transGroup)
			wGruppe.addChild(cube)
			val  axisXLines= new LineArray (2, GeometryArray.COORDINATES)
			axisXLines.setCoordinate(0, new Point3f(-2.0f, 0.0f, 0.0f));
			axisXLines.setCoordinate(1, new Point3f( 2.0f, 0.0f, 0.0f));
			objRoot.addChild(new Shape3D(axisXLines))
			
			val axisYLines = new LineArray(2, GeometryArray.COORDINATES | GeometryArray.COLOR_3)
			axisYLines.setCoordinate(0, new Point3f( 0.0f,-2.0f, 0.0f));
			axisYLines.setCoordinate(1, new Point3f( 0.0f, 2.0f, 0.0f));
			val red = new Color3f(1.0f, 0.0f, 0.0f);
			val green = new Color3f(0.0f, 1.0f, 0.0f);
			axisYLines.setColor(0, green);
			axisYLines.setColor(1, red);			
			objRoot.addChild(new Shape3D(axisYLines))
			
			val blue= new Color3f(0f,0f,1f)
			val axisZLines = new LineArray(2, GeometryArray.COORDINATES | GeometryArray.COLOR_3)
			axisZLines.setCoordinate(0, new Point3f( 0.0f,0f,-2.0f));
			axisZLines.setCoordinate(1, new Point3f( 0.0f,0f, 2.0f));
      axisZLines.setColor(0, green);
      axisZLines.setColor(1, blue);
      objRoot.addChild(new Shape3D(axisZLines))
			

			//yRotate.addChild(wGruppe) 
			objRotate.addChild(wGruppe)
			objRoot.addChild(objRotate)
				//val lineAttrib=new LineAttributes(2,LineAttributes.PATTERN_DASH_DOT,true)
				//val axAppear=new Appearance()
				//axAppear.setLineAttributes(lineAttrib)
				//axis.setAppearance(axAppear)
		}
		catch { 
			case e:Exception=> {
				println("error")
				e.printStackTrace				
			}
		}
		objRoot
	} 
	
	class SimpleBehavior(targetTG:TransformGroup ) extends Behavior {
		val rotation = new Transform3D() 
		val wakeupEvent=new WakeupOnAWTEvent(KeyEvent.KEY_PRESSED)
		var angle = 0d;

		def initialize()= { 
			this.wakeupOn(wakeupEvent);
		}
		// called by Java 3D when appropriate stimulus occurs
		def processStimulus(criteria: java.util.Enumeration[_] ):Unit={	
			for(c<-criteria) c match {					
				
				case `wakeupEvent`=>  wakeupEvent.getAWTEvent()(0) match { 
					case k:KeyEvent => k.getKeyCode match {
						case KeyEvent.VK_F1=> angle -= 0.1
						case KeyEvent.VK_F2 => angle += 0.1;
						case KeyEvent.VK_HOME => angle=0
						case _=> 
					}
					rotation.rotY(angle);
					targetTG.setTransform(rotation);					
				}
				case w:WakeupOnAWTEvent  =>
			}			
			this.wakeupOn(wakeupEvent);
		}
	} 
	
	
}