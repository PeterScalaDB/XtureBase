/**
 * Author: Peter Started:14.05.2011
 */
package management.databrowser
import scala.swing._
import definition.typ._
import server.storage.ServerObjectClass
import client.dialog.ComboBoxEditor
import client.dataviewer.FieldColumnModel
import javax.swing.JComboBox
import scala.collection.JavaConversions._
import javax.swing.table.AbstractTableModel
import scala.collection.mutable.ArrayBuffer
import scala.swing.event.{ButtonClicked,TableRowsSelected}
import java.awt.Color
import javax.swing.BorderFactory
import javax.swing.table.TableCellRenderer
import javax.swing.{JTable,JComponent}
import scala.swing.Table.LabelRenderer
import definition.expression.{StringParser,EMPTY_EX,Expression}


/**
 * 
 */
class AutoCreateDialog(w:Window) extends Dialog(w) {
	val ccModel=new CreateChildTableModel
	val svModel=new StartValuesTableModel
	var currentClass:Option[AbstractObjectClass]=None
	var lastSelectedRow:Option[Int]=None

	val saveBut=new Button("Save")
	val cancelBut= new Button("Cancel")
	val deleteCCEntryBut=new Button("Delete entry ")
	val deleteStartValueBut=new Button("Delete start Value ")

	var propFieldComboEditor:ComboBoxEditor=_
	var fieldComboEditor:ComboBoxEditor=_
	var childTypeComboEditor=new ClassNameEditor(new JComboBox)	

	val ccColMod=new FieldColumnModel{
		createColumn(0,"nr",20)
		createColumn(1,"PropertyField",200)
		createColumn(2,"ChildType",200)		
		//getColumn(2).setCellEditor(childTypeComboEditor)
	}

	val ccTable=new Table {
		val classNameRenderer=new LabelRenderer[Integer]((a:Integer) => {	
			//println(" cnr a:"+a+" "+a.getClass)
			(null, if(a== -1 )""
				else {
					val aValue=a.intValue
					if(aValue<0) "None" 
						else if(aValue==0)	"Any"
							else AllClasses.get.getClassByID(aValue).name
				})
		})	

		val propFieldRenderer=new LabelRenderer[java.lang.Byte]((a:java.lang.Byte) => {
			//println(" pfr a:"+a+" "+a.getClass)
			(null, if(a== (-1).toByte )""
				else {
					val aValue=a.byteValue
					if(aValue<0 || aValue>= currentClass.get.propFields.size) " " 
						else currentClass.get.propFields(aValue).name
				})
		})	
		model=ccModel
		rowHeight=22
		showGrid=true
		selection.elementMode=Table.ElementMode.Row
		selection.intervalMode=Table.IntervalMode.Single
		peer.setAutoCreateColumnsFromModel(false)
		peer.setColumnModel(ccColMod)

		override def editor(row: Int, column: Int)= {
			column match {
				case 1 => propFieldComboEditor
				case 2 => childTypeComboEditor
				case _ => super.editor(row,column)
			}
		}
		override def rendererComponent(isSelected:Boolean, focused:Boolean, row:Int, column:Int):Component= {
			val v = apply(row, column)
			if(v==null)return super.rendererComponent(isSelected,focused,row,column) 
			column match {
				case 1 => return propFieldRenderer.componentFor(this, isSelected, focused,v.asInstanceOf[Byte], row, column)
				case 2 => return classNameRenderer.componentFor(this, isSelected, focused,v.asInstanceOf[Integer], row, column)
				case _=> super.rendererComponent(isSelected,focused,row,column)
			}
		}
	}

	val startValuesTable=new Table {
		rowHeight=22
		model=svModel
		showGrid=true
		selection.elementMode=Table.ElementMode.Row
		selection.intervalMode=Table.IntervalMode.Single
		peer.setAutoCreateColumnsFromModel(false)
		peer.setColumnModel(new FieldColumnModel{
			createColumn(0,"Nr",20)
			createColumn(1,"Data Field",200)
			createColumn(2,"Start value",200)		
		})
		
		var childClass:AbstractObjectClass=_
		
		def setChildClass(cc:Int) = if(cc>0){
			childClass=AllClasses.get.getClassByID(cc)
			fieldComboEditor=new ComboBoxEditor(new JComboBox(new java.util.Vector[(String,Int)](
					for(i <-childClass.fields.indices;val field=childClass.fields(i) ) 
						yield(field.name,i)
			)))			
		}	else childClass=null	

		override def editor(row: Int, column: Int)= {
			if(childClass==null) super.editor(row,column)
			else column match {
				case 1 => fieldComboEditor				
				case _ => super.editor(row,column)
			}
		}
		val fieldRenderer=new LabelRenderer[java.lang.Byte]((a:java.lang.Byte) => {			
			(null, if(a== (-1).toByte )""
				else {
					val aValue=a.byteValue
					if(aValue<0 || aValue>= currentClass.get.fields.size|| childClass==null) " " 
						else childClass.fields(aValue).name
				})
		})
		override def rendererComponent(isSelected:Boolean, focused:Boolean, row:Int, column:Int):Component= {
			val v = apply(row, column)
			if(v==null)return super.rendererComponent(isSelected,focused,row,column) 
			column match {
				case 1 => return fieldRenderer.componentFor(this, isSelected, focused,v.asInstanceOf[Byte], row, column)				
				case _=> super.rendererComponent(isSelected,focused,row,column)
			}
		}
	}

	preferredSize=new Dimension(800,800)


	val mainPanel= new BorderPanel{
		add(new BoxPanel(Orientation.Vertical){	  	 
			contents+= new BorderPanel{
				border= BorderFactory.createTitledBorder("Auto create Child entries:")
				add(new ScrollPane{
					viewportView=ccTable
				},BorderPanel.Position.Center)
				add(new BoxPanel(Orientation.Horizontal){
					contents+=deleteCCEntryBut+=Swing.HGlue
				},BorderPanel.Position.South)
				listenTo(ccTable.selection)
				reactions+= {
					case TableRowsSelected(table,range,live)=> if (!live){						
						var row=table.peer.getSelectedRow
						if(row> -1) selectionChanged(row)
					}	
				}
			}
			contents+= new BorderPanel{
				border= BorderFactory.createTitledBorder("Start values for selected entry:")
				add(new ScrollPane{
					viewportView=startValuesTable
				},BorderPanel.Position.Center)
				add(new BoxPanel(Orientation.Horizontal){
					contents+=deleteStartValueBut+=Swing.HGlue
				},BorderPanel.Position.South)
			}
		} ,BorderPanel.Position.Center)

		add(new BoxPanel(Orientation.Horizontal){
			background=Color.white
			contents+=saveBut+=Swing.HGlue+=cancelBut
			listenTo(saveBut,cancelBut,deleteCCEntryBut,deleteStartValueBut)
			reactions+= {
				case ButtonClicked(`saveBut`)=> saveToClass
				case ButtonClicked(`cancelBut`)=>close
				case ButtonClicked(`deleteCCEntryBut`)=> deleteCCEntry
				case ButtonClicked(`deleteStartValueBut`)=> deleteSVEntry
			}
		},BorderPanel.Position.South)
	}

	contents=mainPanel

	def showDialog(aClass:ServerObjectClass)= {
		startValuesTable.childClass=null
		currentClass=Some(aClass)
		println("Infos:\n"+aClass.ownAutoCreateInfos.mkString("\n"))
		ccModel setValues aClass.ownAutoCreateInfos
		propFieldComboEditor=new ComboBoxEditor(new JComboBox(new java.util.Vector[(String,Int)](
				for(i <-aClass.propFields.indices;val field=aClass.propFields(i) ) 
					yield(field.name,i)
		)))
		
		visible=true
		//ccColMod.getColumn(1).setCellEditor(propFieldComboEditor)
		lastSelectedRow=None
		ccTable.peer.clearSelection
	}

	def selectionChanged(newIx:Int):Unit= if(!ccModel.currentValues.isEmpty){		
		if(newIx<ccModel.currentValues.size){
			svModel.setValues(ccModel.currentValues(newIx).startValues )
			startValuesTable.setChildClass(ccModel.currentValues(newIx).childType)
		}
		lastSelectedRow=Some(newIx)	  
	}
	def setStartValuesToEntry(sv:Seq[(Byte,Expression)]) = for(r<-lastSelectedRow ) {
		ccModel.currentValues(r)=ccModel.currentValues(r).copy(startValues=sv)
	}
	

	def saveToClass = {		 
     TypeDefPanel.theClass=TypeDefPanel.theClass.setAutoCreateInfo(ccModel.currentValues.toList)     
     close
	}

	def deleteCCEntry = ccTable.selection.rows.headOption match {
		case Some(s) => {
			svModel.setValues(Seq.empty)
			lastSelectedRow=None
			ccModel.deleteRow(s)			
		}
		case None =>
	}		

	def deleteSVEntry = startValuesTable.selection.rows.headOption match {
		case Some(s) => svModel.deleteRow(s)
		case None =>
	}


	abstract class DTTableModel[A] extends AbstractTableModel {
		var currentValues=new ArrayBuffer[A]
		                                  var dirty=false

		                                  def setValues(nv:Seq[A])= {
			currentValues.clear
			currentValues ++=nv
			this.fireTableDataChanged()
			dirty=false
		}

		def getRowCount(): Int = currentValues.size+1
		def getColumnCount(): Int = 3  
		override def isCellEditable(row:Int,col:Int)= col>0

		def deleteRow(ix:Int)= {
			currentValues.remove(ix)
			dirty=true
			fireTableDataChanged()
		}
	}


	class CreateChildTableModel extends DTTableModel[AutoCreateInfo]  {	

		def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
			if(rowIndex==currentValues.size) { // last line
				columnIndex match {
					case 0 => " "
					case 1 => (-1).toByte.asInstanceOf[AnyRef]
					case 2 => (-1).asInstanceOf[AnyRef]
				}			 
			} else {
				val el=currentValues(rowIndex)
				columnIndex match {
					case 0 => rowIndex.toString
					case 1 => el.propField.asInstanceOf[AnyRef] 
					case 2 => el.childType.asInstanceOf[AnyRef]
				}
			} 
		}

		override def setValueAt(newValue:Object, rowIndex: Int, columnIndex: Int): Unit = {
			val newVal=newValue match {
				case (_,n:Integer)=> n
				case n:Integer => n
			}
			if(rowIndex < currentValues.size) {
				val oldEntry=currentValues(rowIndex)
				val newEntry=columnIndex match {
					case 1=> oldEntry.copy(propField=newVal.byteValue)
					case 2=> oldEntry.copy(childType=newVal.intValue)
				}
				currentValues(rowIndex)=newEntry
				dirty=true
				this.fireTableDataChanged()
			} else if (rowIndex==currentValues.size) {
				val newEntry=columnIndex match {
					case 1=> AutoCreateInfo(newVal.byteValue,-1,Seq.empty)
					case 2=> AutoCreateInfo(-1,newVal.intValue,Seq.empty)
				}
				currentValues += newEntry
				dirty=true
				this.fireTableDataChanged
			}
		}
	}

	class StartValuesTableModel extends DTTableModel[(Byte,Expression)] {
		def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
			if(rowIndex==currentValues.size) { // last line
				columnIndex match {				
					case 1 => (-1).toByte.asInstanceOf[AnyRef]
					case _ => ""
				}			 
			} else {
				val el=currentValues(rowIndex)
				columnIndex match {
					case 0 => rowIndex.toString
					case 1 => el._1.asInstanceOf[AnyRef] 
					case 2 => el._2.getTerm
				}
			} 
		}

		override def setValueAt(newValue:Object, rowIndex: Int, columnIndex: Int): Unit = 
			if(!ccModel.currentValues.isEmpty){

				def newVal=newValue match {
					case (_,n:Integer)=> n
					case n:Integer => n
				}

				if(rowIndex < currentValues.size) {
					val oldEntry=currentValues(rowIndex)
					val newEntry=columnIndex match {
						case 1=> oldEntry.copy(_1=newVal.byteValue)						
						case 2=> oldEntry.copy(_2=StringParser.parse(newValue.toString))
					}
					currentValues(rowIndex)=newEntry
					setStartValuesToEntry(currentValues.clone)
					dirty=true
					this.fireTableDataChanged()
				} 
				else if (rowIndex==currentValues.size) {
					val newEntry=columnIndex match {

						case 1=> (newVal.byteValue,EMPTY_EX)
						case 2=> ((-1).toByte,StringParser.parse(newValue.toString))
					}
					currentValues += newEntry
					setStartValuesToEntry(currentValues.clone)
					dirty=true
					this.fireTableDataChanged
				}
			}
	}
}