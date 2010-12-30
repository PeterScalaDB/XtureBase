/**
 * Author: Peter Started:25.11.2010
 */
package client.dataviewer

import javax.swing._
import javax.swing.table._
import javax.swing.text._
import javax.swing.event._
import definition.expression._
import definition.typ.DataType
import java.awt.{Insets,Color,BorderLayout,Point,Dimension}
import java.awt.event.{MouseEvent,KeyEvent,ActionEvent,InputEvent,FocusListener,FocusEvent,KeyAdapter}
import java.util.EventObject
/**
 * 
 */
abstract class MultilineEditor(theTable:JTable) extends AbstractCellEditor with TableCellEditor {
	val editColor=new Color(255,255,230)
	val document=new PlainDocument
	var oldPos:Point=null
	val maxEditorHeight=100
	
	val textArea=new JTextArea(document)
	
	//val usedActionKeys=Array(KeyEvent.VK_ENTER,KeyEvent.VK_ESCAPE,KeyEvent.VK_F2,KeyEvent.VK_TAB)
	val usedActionKeyStrokes=Array(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,0),
		KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE,0),
		KeyStroke.getKeyStroke(KeyEvent.VK_F2,0),
		KeyStroke.getKeyStroke(KeyEvent.VK_TAB,0),
		KeyStroke.getKeyStroke(KeyEvent.VK_TAB,InputEvent.SHIFT_DOWN_MASK))
		
	val component:JTextArea = new JTextArea(document) {		
    setBackground(editColor)    
		override protected def processKeyBinding( ks: KeyStroke,e: KeyEvent, condition:Int , pressed:Boolean):Boolean ={
			//System.out.println("Edit process Key Bindings:"+ks+" condition:"+condition)
			if (textArea.hasFocus()) {
				return super.processKeyBinding(ks, e, condition, pressed)
			} else {
				//val superThis=this
				SwingUtilities.invokeLater(new Runnable() {
					def run() {
						if(e.getKeyChar!=KeyEvent.CHAR_UNDEFINED) {							
							if(e.getKeyCode==KeyEvent.VK_TAB/*||e.getKeyCode==KeyEvent.VK_ENTER*/) e.consume
							else textArea.setText(e.getKeyChar.toString)
						} 
						else processKeyBinding(ks, e, condition, pressed);
						textArea.requestFocus();
					}
				})
				return true
			}
		}
    addAncestorListener(new AncestorListener(){
    	def ancestorAdded(e:AncestorEvent ) = {	} 
    	def ancestorMoved(e:AncestorEvent ) = {
    		val newPoint:Point=e.getComponent.getLocation
    		//System.out.println("Moved oldPos:"+oldPos+" newPos:"+newPoint)    		
    	  setPopupLocation(newPoint)
    	}
    	def ancestorRemoved(e:AncestorEvent) = {
    		if(popupContent.isVisible) stopCellEditing
    	}
    })
    addFocusListener(new FocusListener(){
    	 def focusGained(e:FocusEvent)= {
    		 textArea.requestFocus
    	 }
    	 def focusLost(e:FocusEvent)= {}
    })
    
	}		
	
	val popupContent=new PopupContent
	var rootPane:JRootPane=null
	var layeredPane:JLayeredPane=null	
	
	textArea.addKeyListener(new KeyAdapter () {
		override def keyPressed(e:KeyEvent) = {			
			e.getKeyCode match {
				case KeyEvent.VK_ENTER => {
					if(e.isControlDown) {
						textArea.insert("\n",textArea.getCaretPosition());
						e.consume()
					}
					else {						
						val action=textArea.getActionMap.get("selectNextRowCell")
						//System.out.println("action :"+action)
						SwingUtilities.notifyAction(action,KeyStroke.getKeyStroke(e.getKeyCode,e.getModifiers),e,theTable, e.getModifiers)
						e.consume
					}
			    
				}
				case _ =>
			}
		}			
	})
	
	document.addDocumentListener(new DocumentListener {
		def insertUpdate(e:DocumentEvent)={update}   
    def removeUpdate(e:DocumentEvent )={update}   
    def changedUpdate(e:DocumentEvent ) = {update}
    def update= {
    	//System.out.println("Change")
    	SwingUtilities.invokeLater(new Runnable() {
    		def run() { 
    			updateEditorHeight
    		}
    	})
    }		
	})
	
	usedActionKeyStrokes.foreach(registerAction(_))
		
	
	override def shouldSelectCell( e:EventObject  )= 	{		
		true
	}
	
	override def cancelCellEditing()= {
		//System.out.println("cancel ")
		super.cancelCellEditing
		hidePopup
		
	}
	
	override def stopCellEditing() = {
		//System.out.println("stop ")
		val ret=super.stopCellEditing
		hidePopup
		ret
	}
	
	def hidePopup= {
		popupContent.hide		
		SwingUtilities.getRoot(theTable).repaint()
		SwingUtilities.invokeLater(new Runnable{
			def run = theTable.requestFocus
		})
		
	}
	
	def setPopupLocation(pos:Point) = if(layeredPane!=null) {
		val newPos=SwingUtilities.convertPoint(theTable,pos,layeredPane)
		popupContent.setLocation(newPos)
	}
	
	def getTableCellEditorComponent(table:JTable,value: Object, isSelected:Boolean,rowIndex: Int, vColIndex:Int) ={	  
	  //component.setBorder(nofocusBorder)		 
	  		  
	  val bounds=table.getCellRect(rowIndex,vColIndex,true)	  
	  popupContent.setSize(bounds.getSize) // calculate the text height	  
	  oldPos=bounds.getLocation
	  component.setText(setEditorValue(value)) // changes the size also	  
	  if(layeredPane==null) {
	  	rootPane=table.getRootPane
	  	layeredPane=rootPane.getLayeredPane	  	
	  	layeredPane.add(popupContent,(JLayeredPane.POPUP_LAYER) )
	  }		  
	  setPopupLocation(oldPos)
	  popupContent.show
	  popupContent.revalidate
	  textArea.requestFocus
	  component
	}
	
	def setEditorValue(value:Object):String
	
	def updateEditorHeight= {
		val size:Dimension=popupContent.getSize
		var wishHeight=textArea.getPreferredSize.height+4
		//print("WishHeight "+wishHeight)
	  if(wishHeight>maxEditorHeight) wishHeight=maxEditorHeight
	  if(wishHeight!=size.height) {
	  	size.height=wishHeight	  	
	    popupContent.setSize(size)	    
	    textArea.revalidate
	  }	  
	}
	 
	def getCellEditorValue():Object ={
		return component.getText()  
	} 
	
	override def removeCellEditorListener(l:CellEditorListener) = {		
		if(popupContent.isVisible) hidePopup
		super.removeCellEditorListener(l)
	}
	
	override def isCellEditable(evt:EventObject ):Boolean = { 
		if (evt.isInstanceOf[MouseEvent]) {
			evt.asInstanceOf[MouseEvent].getClickCount() >= 2 
		}
		else true		
	}
	
		
	private def registerAction(keyStroke:KeyStroke) = {		
		val actionName=theTable.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
			get(keyStroke)
		if(actionName!=null) {
		  textArea.getInputMap(JComponent.WHEN_FOCUSED).put(keyStroke, actionName)
		  val action=theTable.getActionMap().get(actionName)
		  //System.out.println("instEdit install action :"+actionName +" => "+action)
		  if(action!=null) textArea.getActionMap().put(actionName,new ActionWrapper(action))
		}	
	}
	
	class ActionWrapper(oldAction:Action) extends AbstractAction {	  
		def actionPerformed(e:ActionEvent) = {
			//System.out.println("wrapper actionPerformed "+e+" oldaction:"+oldAction)
			val newEvent=new ActionEvent(theTable,e.getID,e.getActionCommand,e.getWhen,e.getModifiers)
			oldAction.actionPerformed(newEvent)
		}
			
	}
	
	
	class PopupContent extends JScrollPane{
		override def isOptimizedDrawingEnabled=false
		setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)			
		setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.gray),BorderFactory.createLineBorder(getBackground)))
		textArea.setBorder(BorderFactory.createEmptyBorder(0,3,0,3))
		textArea.setLineWrap(true)
		textArea.setWrapStyleWord(true)
		setViewportView(textArea)
		textArea.setBackground(editColor)
	}
}
