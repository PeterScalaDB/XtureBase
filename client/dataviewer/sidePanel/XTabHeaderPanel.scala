/**
 * Author: Peter Started:07.03.2011
 */
package client.dataviewer.sidePanel

/** Header Panel for XTab
 *  background=Color.green
 */

import javax.swing.table.{JTableHeader,TableCellRenderer,DefaultTableCellRenderer}
import java.awt.{Color, Dimension,BorderLayout,Insets,Graphics,Graphics2D}
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import scala.swing._
import java.awt.event.ComponentListener
import java.awt.event.ComponentAdapter
import java.awt.event.{ComponentEvent,MouseEvent,MouseAdapter}
import javax.swing.plaf.ComponentUI
import javax.swing.{JLabel,JTable,JComponent,UIDefaults,UIManager,JPanel,JButton}
import javax.swing.plaf.synth.SynthLabelUI


class XTabHeaderPanel(header:JTableHeader) extends Component {
	/*val defRend=header.getDefaultRenderer
	System.out.println("defaultRenderer :"+defRend)
	val defComp=defRend.getTableCellRendererComponent(null,"",false, false, 0,0).asInstanceOf[JComponent]		
	
	System.out.println ("defUI: "+defUI)*/
  background=Color.green  
  override lazy val peer=header
	xLayoutAlignment=0d
	yLayoutAlignment=1d	
	
	header.setReorderingAllowed(false)
	peer.addComponentListener(new ComponentAdapter(){
		override def componentResized(evt:ComponentEvent) = {
			//println("prefSiz:" +peer.getPreferredSize)
			peer.setMaximumSize(new Dimension(peer.getPreferredSize.width,Short.MaxValue))
			peer.invalidate
		}
	})
	
	
  //val d=new DefaultTableCellHeaderRenderer
	
		
}

class XRenderer(contr:XTabSidePanelController) extends JPanel with TableCellRenderer {
	  setLayout(new BorderLayout())
	  val renderClass=contr.getDefaultHeaderRenderer.asInstanceOf[TableCellRenderer]
	  
	  val render=renderClass.asInstanceOf[DefaultTableCellRenderer]
	  
	  //val label=new DefaultTableCellRenderer	  
    val label=render.getTableCellRendererComponent(null," ",false,false,0,0).asInstanceOf[JLabel]	  
	  val but=new JButton("x")
	  but.setMargin(new Insets(0,0,0,0))
	  but.putClientProperty("JComponent.sizeVariant", "mini");
	  but.updateUI	  
	  //add(label,BorderLayout.CENTER)
    add(but,BorderLayout.EAST)
    def getTableCellRendererComponent(table:JTable, value:Object ,isSelected:Boolean, hasFocus:Boolean,
                                      rowIndex:Int, ColIndex:Int):java.awt.Component ={        
	  	  label.setText(if(value==0) " " else value.toString)
	  	  add(label,BorderLayout.CENTER)
        //label.getTableCellRendererComponent(table,value,isSelected,hasFocus,rowIndex,ColIndex)                         
        return this
    }
	 
    // The following methods override the defaults for performance reasons
    /*override def validate() {}
    override def revalidate() {}*/
    override def firePropertyChange(propertyName:String , oldValue:Object, newValue:Object ) {}
    override def firePropertyChange(propertyName:String , oldValue:Boolean, newValue:Boolean) {}
}
