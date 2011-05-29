/**
 * Author: Peter Started:27.05.2011
 */
package client.dataviewer.view3D



import scala.swing.{Component,Button,GridPanel,Label,Panel}
import definition.typ.{AllClasses,SelectGroup,FieldEditor}
import definition.expression.IntConstant
import definition.data.{Referencable,InstanceData}
import javax.swing.JColorChooser
import scala.swing.event.ButtonClicked
import java.awt.{Dimension,Color}
import client.comm.ClientQueryManager
import client.testUI.ViewTest

/**
 * 
 */
class ColorFieldEditor extends FieldEditor { 

	//val colorChooser=new JColorChooser
	//val widthList=List(0.1,0.15,0.2,0.25,0.35,0.5,0.7)
	val colorBut=new Button	
	
	//var planeClass:Int=0
	
	var allowedClasses:Seq[Int]=Seq.empty
	var classesFields=collection.mutable.HashMap[Int,Byte]()
	
	ClientQueryManager.registerSetupListener(()=>{
		val planeClassID=AllClasses.get.getClassIDByName("Plane")
		val volumeClassID=AllClasses.get.getClassIDByName("Teilvolumen")
  	allowedClasses=List(planeClassID,volumeClassID)
  	classesFields(planeClassID)=3.toByte
  	classesFields(volumeClassID)=1.toByte
  })
	
	var dataList:Seq[SelectGroup[_ <:Referencable]]=Seq.empty	
	
	var currentColor:Option[Color]=None
	
	val backColor=colorBut.background
	val defaultColor=Color.black
	
	def setCurrentColor(newColor:Option[Color])= {
		currentColor=newColor
		newColor match {
			case Some(color)=> colorBut.background=color
			case _ => colorBut.background=backColor
		}
	}
	
	val panel=new GridPanel(1,2) {
		contents +=new Label("Farbe:")
		contents +=colorBut
		preferredSize=new Dimension(70,32)
		listenTo(colorBut)		
		reactions+={
			case e:ButtonClicked=> 	{				
				if(dataList!=null){
					val color=JColorChooser.showDialog(this.peer,"Farbe auswählen",currentColor.getOrElse(defaultColor))
					if(color!=null) {
						setCurrentColor(Some(color))
						val instList=dataList.flatMap(_.children.filter(inst =>allowedClasses.contains(inst.ref.typ )))
						if(!instList.isEmpty){						
							val typedMap=instList.groupBy(_.ref.typ)
							//println("typedMap:"+typedMap)
							for(typ <-typedMap.keySet)
								ClientQueryManager.writeInstancesField(typedMap(typ),classesFields(typ),new IntConstant(color.getRGB))
						}
					}						
				}				
			}
		}
		
	}
	
  def getPanel:Panel=panel
  
	def setData(data:Seq[SelectGroup[_ <: Referencable]])= {
    dataList=data
    if(data!=null) { 
    	var newColor:Option[Color]=None       
    	for(group <-data;el <-group.children) {
    		el match {
    			case inst:InstanceData => if(allowedClasses.contains(inst.ref.typ)) {
    				newColor=Some(new Color(inst.fieldValue(classesFields(inst.ref.typ)).toInt))
    			}
    			case a:AxisShape => {
    				newColor=Some( a.color)
    			}
    			case v:VolumeShape => {
    				newColor=Some( v.prism.color)
    			}
    			case _ =>
    		}      		      		
    	}
    	setCurrentColor(newColor)
    } 
	}  
    
  
}