/**
 * Author: Peter Started:07.03.2011
 */
package client.dataviewer.sidePanel

import scala.swing.Panel
import java.awt.{Color,Dimension}
import scala.swing._
import javax.swing.JViewport

/** Main Panel for XTab
 * 
 */
class XTabMainPanel(table:Table) extends Component with Container  {
   background=Color.blue
   //preferredSize=new Dimension(100,20)
   xLayoutAlignment=0d
   yLayoutAlignment=0d
   override lazy val peer=new JViewport with SuperMixin{
  	 override def getPreferredSize = getView.getPreferredSize
  	 override def getMaximumSize = getView.getPreferredSize
   }
   peer.setView(table.peer)
   def contents=List(table)
}