/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer
import client.comm._
import collection.generic.{GenericCompanion}
import definition.data._
import definition.expression._
import definition.typ._
import javax.swing._
import javax.swing.event._
import javax.swing.table._
import java.awt.{Color, Dimension, Font}
import java.awt.event.{MouseAdapter,MouseWheelListener,MouseWheelEvent,KeyEvent}
import scala.swing._
import scala.swing.event._
import javax.swing.BorderFactory
import javax.swing.border._
import sidePanel._
import client.dataviewer.sidePanel.ControllerContainer



/** table model for a table showing instances of a certain type
 * 
 */
class TypeTableModel(val typ:Int,val propMod:PropertyModel) extends AbstractTableModel with ControllerContainer {
	val objClass=AllClasses.get.getClassByID(typ).asInstanceOf[ClientObjectClass]
	var dataList:Seq[InstanceData]= Seq.empty
	val tableFont=new Font("Arial",0,13)
	val tableTypeFont=new Font("Arial",Font.ITALIC,11)
	val buttonBackgroundColor=new Color(220,220,220)	
	var selfSelectChanged=false
	var selfAdded=false
	val selectedInstances=new SelectList(dataList)
	var dragColumn:Int= -1
	var dragColumnNewPos:Int=0
	
	val enumColumns=if(objClass.enumFields==null) Seq.empty else objClass.enumFields.map(a=>a._1)
	
	var wishSelection:Seq[Int]=Seq.empty // what items should be selected after a refresh (for move and copy)
	
	val defaultRowHeight=20
	
	val transferHandler=new TableTransferHandler(this)

	val listLock=new Object
	
	val classLabel=new Label("   "+AllClasses.get.getClassByID(typ).name)
	
	var clickedRow= -1
	var clickedCol= -1
	
	
	var editActionNotDone:java.awt.event.ActionEvent=null
	var oldEnterAction:javax.swing.Action=null
	
	val sideBarPanel=new BoxPanel(Orientation.Vertical)
	
	
	lazy val emptyHeaderPanel=new BoxPanel(Orientation.Horizontal ) {
		reactions += {
			case ButtonClicked(e) => openSideBarController(e.text)
		}
	}
	
	lazy val sideControllerList=SPControllerList.generateList(objClass)
	
	var currentSideBarController:Option[SidePanelController]=None
	
	
	
	classLabel.horizontalAlignment=Alignment.Left
	classLabel.xLayoutAlignment=0d
	classLabel.font=tableTypeFont
	classLabel.border=BorderFactory.createLineBorder(classLabel.background,2)
  
	def isEmpty=dataList.isEmpty
	
	val table:Table=new Table(){				
		
		autoResizeMode=Table.AutoResizeMode.Off
		//selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.Row 
		peer.setAutoCreateColumnsFromModel(false)
		//peer.setFillsViewportWidth(true)
		//peer.putClientProperty("terminateEditOnFocusLost", true)
		peer.setTransferHandler(transferHandler)
		peer.setDragEnabled(true)
		peer.setDropMode(DropMode.ON)		
		rowHeight=defaultRowHeight
		font=tableFont

		listenTo(selection)	
		listenTo(mouse.clicks,this,keys)
		reactions += {
			case TableRowsSelected(table,range,live) => {			
				if (!live&& ! selfSelectChanged) listLock.synchronized  { 
					//System.out.println("range:"+range+" rows:"+selection.rows+" DataList:"+(if(dataList==null) "null" else dataList.size)
					//	+ " buf:"+(if(selectedInstances.buf!=null)selectedInstances.buf.size else "null"))
					selectedInstances.setFilter(peer.getSelectedRows)										
					propMod.mainController.selectionChanged(TypeTableModel.this,propMod,selectedInstances)
				}
			}				
			/*case e: MouseClicked => 
			if(dataList!=null && peer.columnAtPoint(e.point)==0 && e.clicks==1 
					&& e.triggersPopup == false &&  (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1) )	{
				val row= peer.rowAtPoint(e.point)
				if(row>=0 && row<dataList.size) 
			}*/
			case e: MousePressed => if(e.peer.getButton== java.awt.event.MouseEvent.BUTTON1){
				clickedCol=peer.columnAtPoint(e.point)
				clickedRow=peer.rowAtPoint(e.point)
				table.repaint
			}
			
			case e: MouseReleased => { if(dataList!=null && peer.columnAtPoint(e.point)== 0 && clickedCol==0 && 
					peer.rowAtPoint(e.point)==clickedRow && clickedRow>=0 && clickedRow < dataList.size&& e.triggersPopup==false &&
					e.peer.getButton== java.awt.event.MouseEvent.BUTTON1)
				    listLock.synchronized { propMod.mainController.openChild(dataList(clickedRow).ref)}
				  clickedRow= -1
				  clickedCol= -1
				  table.repaint
		  	}
			
			case e:FocusGained =>propMod.focusGained
			case e: KeyPressed => {
				if(e.peer .getKeyChar==KeyEvent.CHAR_UNDEFINED) {
					//System.out.println("Special Key:"+e.peer.getKeyCode)
					// if there are no mappings for that key, ignore it
					if(table.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
							get(KeyStroke.getKeyStroke(e.peer.getKeyCode,0,false))==null){
						//System.out.println("ignore")
						e.peer.consume									
					}
				}
			}			
		}
		model=TypeTableModel.this
		peer.setColumnModel(TableHeaderMap.getColumnModel(typ))		
		showGrid=true
		gridColor=Color.gray
		// prevent first column from getting moved
		peer.getColumnModel().addColumnModelListener(new TableColumnModelListener(){ 
			def columnAdded(e:TableColumnModelEvent ) {}
			def columnMarginChanged(e:ChangeEvent ) {} 
			def columnMoved(e:TableColumnModelEvent){ 
				if (dragColumn == -1) 
					dragColumn = e.getFromIndex();
				dragColumnNewPos = e.getToIndex(); 
			}
			def columnRemoved(e:TableColumnModelEvent ) {}
			def columnSelectionChanged(e:javax.swing.event.ListSelectionEvent ) {} 
		});
		peer.getTableHeader().addMouseListener(new java.awt.event.MouseAdapter() { 
			override def mouseReleased(e:java.awt.event.MouseEvent) {
				if (dragColumn != -1 && (dragColumn == 0 || dragColumnNewPos == 0)) 
					peer.moveColumn(dragColumnNewPos, dragColumn); 
				dragColumn = -1;  dragColumnNewPos = -1; 
			} 
		});	
		
		
		// setup renderers
		val firstButRend=new FirstColumnRenderer(TypeTableModel.this)
		val ftcr = new Table.AbstractRenderer[String, FirstColumnRenderer](firstButRend) {
			def configure(t: Table, sel: Boolean, foc: Boolean, o: String, row: Int, col: Int) =     
				component.config(sel,foc,o,row)  
		}
		
		val instRenderer=new InstanceRenderer(AllClasses.get.getClassByID(typ))
		val itcr = new Table.AbstractRenderer[Expression, InstanceRenderer](instRenderer) {
			def configure(t: Table, sel: Boolean, foc: Boolean, o: Expression, row: Int, col: Int) =     
				component.config(t,sel,foc,o,row,col)
		}
		val etcr = new Table.AbstractRenderer[Tuple2[String,Int], EnumRenderer](new EnumRenderer) {
			def configure(t: Table, sel: Boolean, foc: Boolean, o:Tuple2[String,Int], row: Int, col: Int) = {    
				component.prepare(t,sel,o,row)
  }
}

		
		override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
			//FIND VALUE
			val modCol=peer.convertColumnIndexToModel(col)
			val v=model.getValueAt(row,modCol)
			//val v = model.getValueAt(	peer.convertRowIndexToModel(row),	modCol)
			if(col==0) {
				if(row>=getRowCount-1)super.rendererComponent(sel,foc,row,col)
				else ftcr.componentFor(this,sel,foc,if(v==null) "" else v.toString, row, col)
			}
			else v match {
				case ve:Expression => itcr.componentFor(this,sel,foc,ve,row,modCol)
				case vt:Tuple2[_,_] => etcr.componentFor(this,sel,foc,v.asInstanceOf[Tuple2[String,Int]],row,modCol)
				case _=> super.rendererComponent(sel,foc,row,col)
			}			 
		}
		
		super.rendererComponent(false,false,0,0).font=tableFont
		
		override def editor(row: Int, column: Int)= {
			if(column==0)null
			else {
				val edit=peer.getColumnModel.getColumn(column).getCellEditor
				if(edit!=null)edit
				else instEditor
			}
		}
		
	}
	
		
	replaceKeyAction(KeyEvent.VK_ENTER,(oldAction)=>{
		oldEnterAction=oldAction
		new javax.swing.AbstractAction() {
			def actionPerformed(e:java.awt.event.ActionEvent)= {
				//System.out.println("Enter Abgefangen "+table.peer .isEditing)
				if(!table.peer.isEditing) {
					oldAction.actionPerformed(e)
					editActionNotDone=null
				}
				else {
					
					instEditor.stopCellEditing
					editActionNotDone=e
				}			
			}
		}
	})
	
	replaceKeyAction(KeyEvent.VK_ESCAPE,(oldAction)=> {
		new javax.swing.AbstractAction() {
			def actionPerformed(e:java.awt.event.ActionEvent)= {
				//System.out.println("Escape :"+table.peer .isEditing)
				if(table.peer.isEditing)
					oldAction.actionPerformed(e)				
			}
		}
	})
	
	val instEditor=new MultilineEditor(table.peer)	{
		def setEditorValue(value:Object) = {
			if (value.isInstanceOf[Expression]){
				val expr=value.asInstanceOf[Expression]
				if(expr.getType==DataType.StringTyp) expr.toString
				else expr.getTerm
			}
			else if(value==null)"" else value.toString
		}
	}
	table.peer.setDefaultEditor(classOf[definition.expression.Expression],instEditor)
	//println("CellEditor:" +table.peer .getColumnModel.getColumn(1).getCellEditor)
	
	
	
	val leftPanel= new Panel with SequentialContainer.Wrapper {
		//border=BorderFactory.createLineBorder(Color.cyan,2)
    yLayoutAlignment=0d
		val viewportWrapper=new  Component with Container {	
			xLayoutAlignment=0d
			override lazy val peer=new JViewport with SuperMixin{
				override def getPreferredSize = getView.getPreferredSize
				override def getMaximumSize = getView.getPreferredSize
			}
			peer.setView(table.peer)
			def contents=List(table)
		}
		val headerWrapper= new Component {
			override lazy val peer=table.peer.getTableHeader
			xLayoutAlignment=0d
		}		
		xLayoutAlignment=0d
		contents+=classLabel
		//contents+=Swing.VStrut(4)
		contents+=headerWrapper
		contents+=viewportWrapper	
		
		//override def maximumSize=new Dimension(400,preferredSize.height)
		//override def preferredSize=new Dimension(400,table.preferredSize.height+classLabel.preferredSize.height+headerWrapper.preferredSize.height)
		override lazy val peer = {
			val p = new javax.swing.JPanel with SuperMixin {
				override def getPreferredSize=new Dimension(table.preferredSize.width,table.preferredSize.height+classLabel.preferredSize.height+headerWrapper.preferredSize.height)
				override def getMaximumSize=getPreferredSize
			}
			val l = new javax.swing.BoxLayout(p, Orientation.Vertical.id)
			p.setLayout(l)    
			p
		}
	}
	
	val scroller= new BoxPanel(Orientation.Horizontal){	
		//border=BorderFactory.createLineBorder(Color.yellow,3)
		contents+=leftPanel
		sideBarPanel.yLayoutAlignment=0d
		sideBarPanel.maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
		sideBarPanel.xLayoutAlignment=0d
	  contents+=sideBarPanel
	  //contents+=Swing.HGlue
	  /*override def preferredSize=new Dimension(leftPanel.preferredSize.width+sideBarPanel.preferredSize.width,
	  	leftPanel.preferredSize.height)*/		
		minimumSize=new Dimension(0,0)
		//override def maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
	}
	
		
	/** Checks what sidebar controllers can be active for this kind of data
	 *
	 */
	def updateControllers:Unit = listLock.synchronized {
		  currentSideBarController match {
		  	case Some(contr) => {
		  		contr.notifyRowsChanged
		  	}
		  	case None => {
		  		shutDown
		  		propMod.mainController.currentSidePanelController match {
		  			case Some(oContr)=>{
		  				println("oldContr:"+oContr+" "+sideControllerList.size)
		  				for (c<-sideControllerList) {
		  					println("C:"+c+ " "+propMod.mainController.ref+" " +c.parentsFits(this,propMod.mainController.ref))
		  					if (c.parentsFits(this,propMod.mainController.ref)){
		  						println("contr fits :"+c.getClass==oContr.getClass)
		  						if(c.getClass==oContr.getClass) {
		  							ClientQueryManager.runSw{openSideBarController(c.panelName)}
		  							
		  							return
		  						}
		  					}
		  				}
		  				//propMod.mainController.currentSidePanelController=None
		  			}
		  			case None => 
		  		}		  		
		  		showEmptyHeaderPanel		  		
		  	}
		}
				
		
	}
	
	private def showEmptyHeaderPanel = {
			sideBarPanel.contents.clear
			emptyHeaderPanel.contents.clear()
			println("showEmptyPanel :"+sideControllerList.size)
			for (c<-sideControllerList;if (c.parentsFits(this,propMod.mainController.ref))) {			
				val but=new Button(c.panelName)
				emptyHeaderPanel.listenTo(but)
				emptyHeaderPanel.contents+=but
			}
			emptyHeaderPanel.contents+=Swing.HGlue
			sideBarPanel.contents+=emptyHeaderPanel
	}
	
	/** is called by the emptyHeaderPanel when a Button is clicked
	 * opens the custom panels in the sidebar
	 * 
	 * @param name name of the Controller choosen
	 */
	private def openSideBarController(name:String):Unit = {
		currentSideBarController=None
		for(contr <-sideControllerList.find(_.panelName==name)) {
			currentSideBarController=Some(contr)			
			sideBarPanel.contents.clear
			val header=contr.headerComp
			sideBarPanel.contents+=header			
			sideBarPanel.contents+=contr.mainComp
			println("cl:"+classLabel.preferredSize.height+" th:"+table.peer.getTableHeader.size.height)
			header.preferredSize=new Dimension(1,classLabel.preferredSize.height+table.peer.getTableHeader.size.height)
			header.maximumSize=new Dimension(Short.MaxValue,classLabel.preferredSize.height+table.peer.getTableHeader.size.height)			
			contr.openPanel(propMod.mainController.ref, objClass,this)
			propMod.mainController .currentSidePanelController =Some(contr)
			propMod.mainController .currentSidePanelControllerWasUsed =true
		}
		sideBarPanel.peer.invalidate
		scroller.peer.revalidate
		scroller.peer.repaint()
	}
	
	/** shuts down eventually open sidebar controllers 
	 * 
	 */
	def shutDown = {
		removeSideBar
	}
	
	private def removeSideBar = {
		for(c <- currentSideBarController)
			c.closePanel
		currentSideBarController=None
		sideBarPanel.contents.clear
	}
	
	/** is called from the sideBarController when it's close button was clicked
	 * 
	 */
	def closeSideBar = {
		removeSideBar		
		sideBarPanel.contents+=emptyHeaderPanel
		propMod .mainController.currentSidePanelController=None
		scroller.repaint
	}
	
	
	def replaceKeyAction(keyCode:Int,func: (javax.swing.Action )=>javax.swing.Action)= {
		val aName=table.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
			get(KeyStroke.getKeyStroke(keyCode,0,false))
		val oldAction=	table.peer.getActionMap().get(aName)
		//System.out.println("tablemod replace name:"+aName+" oldAction:"+oldAction)
		val newAction= func(oldAction)
		table.peer .getActionMap().put(aName,newAction)
	}
	
	
	def getParentRef=propMod.mainController.ref
	def getPropField= propMod.ownerRef.ownerField 
	    
  /** load the current values in the table model
   * 
   * @param data the new data
   * @param selectInstance what instance should be selected (when moving up the path)
   * @param onlyRefresh // true= this is only a refresh call after a move, false: it is first time called for loading
   */
	def setDataList(data:Seq[InstanceData],selectInstance:Option[Reference],onlyRefresh:Boolean) =  {
		listLock.synchronized {
			
			clickedRow= -1
		  clickedCol= -1
			//System.out.println("tableMod set Data "+data.mkString)
			dataList=data			
			selfSelectChanged=false
			selfAdded=false
			selectedInstances.buf=data
			selectedInstances.setFilter(Array())
			if(wishSelection.isEmpty)
				wishSelection=table.selection.rows.iterator.toSeq
			fireTableDataChanged()		
			selectInstance match {
				case Some(ref) =>if(ref.typ == typ){
					val ix= dataList.findIndexOf(_.ref==ref)
					if(ix>=0){
						table.selection.rows+=ix
						//System.out.println("catch Focus "+typ)
						table.requestFocus
					}
				}
				case None => if(onlyRefresh) {
					wishSelection.foreach(a => if(a<data.size) table.selection.rows+= a)
				}
			}
			wishSelection=Seq.empty			
		}
		Thread.`yield`()
		listLock.synchronized{
			updateControllers
		}
		//setupColumns()		
	}
	
	
	
	def calcSize() = {				
	}

	def changeInstance(newInst:InstanceData):Unit = listLock.synchronized {
		//System.out.println("tablemod change inst: "+newInst.ref)
		val pos = { var ret= -1
			for(i <-dataList.indices;if(dataList(i).ref==newInst.ref)) {ret=i} 
			ret
		}
		//System.out.println("change "+newInst.ref+ " size:"+dataList.size+ " pos:"+pos+ " list:"+dataList.mkString+ "  "+ Thread.currentThread)
		if(pos<0) System.out.println("prop "+getPropField+" Table typ: "+typ+" Change Instance "+newInst.ref+" not found ! " + dataList.size+" "+Thread.currentThread)
		else  { 
			//System.out.println("change selfadded:"+selfAdded+" EditActionNotDone:"+editActionNotDone)
			dataList=dataList.updated(pos,newInst)
			selectedInstances.buf=dataList
			/*propMod.runSw*/fireTableRowsUpdated(pos,pos)
			if(editActionNotDone!=null)	{
				oldEnterAction.actionPerformed(editActionNotDone)
				editActionNotDone=null
			}
				
		}	
	}

	def addInstance(newInst:InstanceData) = listLock.synchronized {
		//System.out.println("tablemod add inst: "+newInst.ref)
		if(dataList==null) {
			dataList=IndexedSeq(newInst)
			//setupColumns()
		}
		else {
			//if(pos<0 || pos >=dataList.size)
				dataList=dataList :+ newInst
				//else  dataList=(dataList.take(pos):+newInst)++ dataList.drop(pos)
		}
		selectedInstances.buf=dataList
		//System.out.println("added "+dataList.size+" "+Thread.currentThread+ " "+table.selection.rows)
		val newSize=dataList.size
		
		fireTableRowsInserted(newSize,newSize)
		for(c <-currentSideBarController) c.notifyRowsChanged
		calcSize()
		//System.out.println(" add selfadded:"+selfAdded+" EditActionNotDone:"+editActionNotDone)
		if(selfAdded){ 
			propMod.mainController.selectionChanged(TypeTableModel.this,propMod,Array(newInst))
			selfAdded=false
			if(editActionNotDone!=null)	{
				oldEnterAction.actionPerformed(editActionNotDone)
				editActionNotDone=null
			}
		}
		
			//table.peer.setRowSelectionInterval(newSize-1,newSize-1)}
	}

	def removeInstance(ref:Reference) = listLock.synchronized{	
		//System.out.println("tablemod remove inst: "+ref)
		val pos=dataList.indexWhere(_.ref==ref)
		if(pos<0) System.out.println("Remove Instance "+ref+" not found !")
		else {
			dataList=dataList filterNot(_.ref ==ref)
			selectedInstances.buf=dataList
			calcSize
			/*propMod.runSw*/fireTableRowsDeleted(pos,pos)
			for(c <-currentSideBarController) c.notifyRowsChanged
		}
	}


	def getRowCount= listLock.synchronized{
		 //System.out.println("get size "+(dataList.size+1)+ " " +Thread.currentThread)
		if(dataList!=null) dataList.size+1
		else 0
	}

	def getColumnCount= listLock.synchronized{
		objClass.fields.size+1
	}

	def getValueAt(row:Int,col:Int) = listLock.synchronized{
		if(dataList!=null&& row<dataList.size) {
			val el=dataList(row)
			if(col==0) { 
				var retStr=if (el.hasChildren) "+" else " "
				if(el.secondUseOwners.size>0) 
					retStr=retStr+" ·"
				retStr
			} // childInfo in column 0
			else {	
				val found=if(objClass.enumFields==null) -1 else objClass.enumFields.findIndexOf(_._1==col-1)
				if(found> -1) objClass.enumFields(found)._2.getElem(el.fieldValue(col-1).toInt)
				else el.fieldData(col-1)				
			}
		}
		else null
	}
	
	def getRowReference(row:Int):Option[Reference] = listLock.synchronized {
		if(dataList!=null&& row<dataList.size) Some(dataList(row).ref)
		else None
	}

	override def setValueAt(aValue: Object, rowIndex: Int, columnIndex: Int): Unit =		
		if(dataList!=null&& columnIndex >0)  listLock.synchronized {  
			
			val expr= if(objClass.enumFields.exists(_._1==columnIndex-1)) 
				IntConstant(if(aValue==null) 0 else aValue.asInstanceOf[(String,Int)]._2) // enumeration 
				else parseValue(columnIndex,aValue) 		
			if(rowIndex==dataList.size) { // create new
				selfAdded=true
				val id=ClientQueryManager.createInstance(typ,Array(propMod.getOwnerRef))				
				ClientQueryManager.writeInstanceField(Reference(typ,id),(columnIndex-1).toByte,expr)											
			}
			else {
				val ref=dataList(rowIndex).ref
				ClientQueryManager.writeInstanceField(ref,(columnIndex-1).toByte,expr)
			} 		
			
		}  	


	override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = { 
		columnIndex>0 
	}

	override def getColumnName(col:Int) = listLock.synchronized {
		if(col==0) "ch" else objClass.fields(col-1).name
	}
	
	override def getColumnClass(col:Int):java.lang.Class[_] =  {
		//println("get Column class for class "+objClass.name+" "+objClass.enumFields.mkString)
		if(col==0) classOf[String] else {
			if(objClass.enumFields!=null && objClass.enumFields.exists(_._1==col-1)) classOf[(String,Int)]
		  else classOf[definition.expression.Expression]
		}
	}


	def parseValue(columnIndex:Int,value:Object):Expression = {
		if(value==null)Expression.generateNullConstant(objClass.fields(columnIndex-1).typ) else
			if (objClass.fields(columnIndex-1).typ==DataType.StringTyp)
				try {
					StringParser.parse( value.toString) 
				} 
		catch {
			case _ => new StringConstant(value.toString)				
		}
		else StringParser.parse( value.toString) // throw exception when fail
	}


	def deselect() = listLock.synchronized{
		//System.out.println("deselect " +typ+ " rows:"+table.selection.rows)
		if(dataList!=null && (! table.selection.rows.isEmpty)) {
			if(table.peer .isEditing) instEditor.cancelCellEditing
			selfSelectChanged=true
			table.peer.clearSelection
			selfSelectChanged=false
			selectedInstances.setFilter(Array())
		}
	}

	


	class SelectList[A](var buf: Seq[A],private var filterSet:Array[Int]=Array()) 
	extends collection.immutable.IndexedSeq[A] {
		def length = if(buf==null) 0 else filterSet.size
		def apply(idx: Int) ={ //System.out.println ("buf size:"+buf.size+ " "+" idx:"+idx+" filterSet:"+filterSet.mkString+" "+Thread.currentThread)
			val bufIx=filterSet(idx)
			if(bufIx<buf.size)
			buf.apply(filterSet(idx))
			else null.asInstanceOf[A]
		}
		def setFilter(newFilter:Array[Int]) = if(buf!=null && newFilter.contains(buf.size)) {
			filterSet=newFilter.take(newFilter.size-1)
		} else filterSet=newFilter
	 }

  class FirstColumnRenderer(mod:TypeTableModel) extends Label {
  	val raisedBorder=BorderFactory.createRaisedBevelBorder();
  	val loweredBorder=BorderFactory.createLoweredBevelBorder();
  	border=BorderFactory.createRaisedBevelBorder();
  	
  	override def revalidate= {}
  	super.background=buttonBackgroundColor
  	super.foreground=Color.black
  	override def background_=(c: Color) = {				
		}
  	override def foreground_=(c: Color) = {				
		}
  	
		def config( isSelected: Boolean, focused: Boolean, a: String, row: Int) {  		
  	  text=a	
  	  super.background=if(clickedCol==0&&clickedRow==row)Color.lightGray else buttonBackgroundColor
  	  border=if(clickedCol==0&&clickedRow==row)loweredBorder else raisedBorder
		}
	}
}

class EnumRenderer extends Label {
  	override def revalidate= {}
  	def prepare(t:Table,isSelected:Boolean,o: Tuple2[String,Int],row:Int) {
  		horizontalAlignment=Alignment.Left
  		text = "· "+o._1 //or whatever
  		background=if(isSelected)  t.selectionBackground 
  		else  if (row % 2 == 0)InstanceRenderer.alternateColor 
  		else Color.white
  	}
  }