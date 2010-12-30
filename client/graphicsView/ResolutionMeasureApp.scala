/**
 * Author: Peter Started:03.10.2010
 */
package client.graphicsView

import scala.swing._
import scala.swing.event._
import java.awt.{Dimension,Graphics,Graphics2D,Color,RenderingHints,BasicStroke}
import java.awt.geom._

/**
 * 
 */
object ResolutionMeasureApp extends SimpleSwingApplication  {
	val widthEdit=new TextField("1200")
	val heightEdit=new TextField("800")
	val mmWidthEdit=new TextField("")
	val mmHeightEdit=new TextField("1")
	val xResLabel=new Label("")
	val yResLabel=new Label("")
	
	val editPanel= new GridPanel(6,2) {		
		preferredSize=new Dimension(400,200)
		contents+= new Label("box Width x (pix):")+=widthEdit+=
								new Label("box Height y(pix):")+=heightEdit+=
								new Label("box Width x (mm):")+=mmWidthEdit+=
								new Label("box Width y (mm):")+=mmHeightEdit+=
								new Label("x-resolution(pix/mm):")+=xResLabel+=
								new Label("y-resolution(pix/mm):")+=yResLabel
	}
	
	val showBox= new BorderPanel {
		  background=Color.white
		  opaque=true
		  preferredSize=new Dimension(1200,800)
		  maximumSize=preferredSize
		  override def paintComponent(g:Graphics2D)= {		  	
		  	super.paintComponent(g);
		  	g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON ))
		  	//g.setRenderingHints(new RenderingHints(RenderingHints.KEY_STROKE_CONTROL,RenderingHints.VALUE_STROKE_NORMALIZE ))
		  	//g.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,RenderingHints.VALUE_INTERPOLATION_BICUBIC ))
		  	g.setPaint(Color.black)
		  	g.setStroke(new BasicStroke(0.2f))
		  	g.draw(new Line2D.Double(0,0,1000,500))
		  	g.draw(new Line2D.Double(1000,0,1000,500))
		  	g.draw(new Line2D.Double(0,500,1000,500))
		  }
	}
	
	val mainPanel = new BorderPanel() {	
		//contents :+ (editPanel ->BorderPanel.Position.North)
		add(editPanel,BorderPanel.Position.North)
		add(new BoxPanel (Orientation.Vertical )  {
			contents += new BoxPanel(Orientation.Horizontal) {
				contents += showBox
				contents += Swing.HGlue
				background=Color.green
			}
			contents += Swing.VGlue
			//preferredSize=new Dimension(100,40)
			background=Color.red
		}
		,BorderPanel.Position.Center)
		listenTo(widthEdit,heightEdit,mmWidthEdit,mmHeightEdit)
		reactions += {
			case EditDone(`widthEdit`) =>{
				val nw=textToDouble(widthEdit.text,400)
				System.out.println("nw:"+nw)
				val nsize=new Dimension(nw.toInt,showBox.preferredSize.height)
				showBox.minimumSize=nsize
				showBox.preferredSize=nsize
				showBox.maximumSize=nsize
				showBox.revalidate
				//showBox.invalidate
			}																
			case EditDone(`heightEdit`) => {
				val nh=textToDouble(heightEdit.text,200)
				System.out.println("NH:"+nh)
				val nsize=new Dimension(showBox.preferredSize.width,nh.toInt)
				showBox.minimumSize=nsize
				showBox.preferredSize=nsize
				showBox.maximumSize=nsize
			}
			case EditDone(`mmWidthEdit`) => {
				val hscale=showBox.maximumSize.width.toDouble/textToDouble(mmWidthEdit.text,-1)
				xResLabel.text=hscale.toString+" dot pitch="+1/hscale+"mm"
			}
			case EditDone(`mmHeightEdit`) => {
				val hscale=showBox.maximumSize.height.toDouble/textToDouble(mmHeightEdit.text,-1)
				yResLabel.text=hscale.toString+" dot pitch="+1/hscale+"mm"
			}
		}
		//contents :+ (editPanel -> )
	}
	
	def textToDouble(text:String,default:Int) = try {
				text.toDouble
			} catch {
				case e:Exception => default
			}
	
	
	
	val top = new MainFrame ()
	{		
		title="Resolution-Test"
		contents = mainPanel
		bounds=new Rectangle(200,200,900,600)		
	}	
	
	
}