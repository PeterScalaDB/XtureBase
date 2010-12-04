/**
 * Author: Peter Started:28.11.2010
 */
package management.databrowser

import scala.swing._
import definition.typ._
import definition.data._
import java.awt.{Dimension,Color}
import javax.swing.{BorderFactory,JTable,JLabel,JComboBox,DefaultCellEditor,DefaultComboBoxModel,JTextField}
import javax.swing.border._
import scala.swing.event._
import javax.swing.table._
import server.storage._
import client.dataviewer.MultilineEditor
import client.dialog.ReactiveTextField
import transaction.handling.SessionManager
import server.config.FSPaths
import scala.collection.JavaConversions._

/**
 * 
 */
object TypeDefPanel extends ScrollPane {
	
	//var usedIDs=collection.mutable.HashSet[Int]()
	
	var isCreating:Boolean=false
	var theClass:ServerObjectClass =EmptyServerClass		
	val nameEdit=new ReactiveTextField((s)=> theClass.name =s)
	val descriptionEdit=new ReactiveTextField((s)=> theClass.description =s)
	val idEdit=new ReactiveTextField((s)=>theClass.id=s.toInt)
	val classesListview = new ListView[(Int,String)]
	classesListview.selection.intervalMode=ListView.IntervalMode.Single
	val superClassesListview = new ListView[(Int,String)]
	superClassesListview.selection.intervalMode=ListView.IntervalMode.Single
	val addSuperClassBut=new Button("add >")
	val removeSuperClassBut= new Button("< remove ")
	val disableComps=List(idEdit,addSuperClassBut,removeSuperClassBut)	
	val saveBut=new Button("Save changes")
	val inheritedFieldMod=new FieldDefTableModel(false)
	val ownFieldMod=new FieldDefTableModel(true)
	val vect=new java.util.Vector[DTWrap](DataType.wrappedValues)  	
  val typeComboBox=new JComboBox(vect)
	val typeEditor=new TypeEditor(typeComboBox)
	val fieldColMod=new FieldColumnModel{
    	createColumn(0,"Name",120)
    	createColumn(1,"Typ",70)
    	createColumn(2,"locked",50)    	
    	createColumn(3,"term",50)  
    	createColumn(4,"editor",100)
    	createColumn(5,"startValue",100)
    	createColumn(6,"Format",80)
    	getColumn(1).setCellEditor(typeEditor)
    }
	val inheritedFieldTable=new FieldTable(inheritedFieldMod,fieldColMod)
	
	val ownFieldTable=new FieldTable(ownFieldMod,fieldColMod)
	//ownFieldTable.peer.getColumnModel.getColumn(1).setCellEditor(typeEditor)
	val inheritedPropMod=new PropFieldTableModel(false)
	val ownPropMod= new PropFieldTableModel(true)
	val inheritedPropTable=new FieldTable(inheritedPropMod)
	inheritedPropTable.background=Color.lightGray
	val ownPropTable=new FieldTable(ownPropMod)
	val classNameRenderer=new ClassNameRenderer
	inheritedPropTable.peer.setDefaultRenderer(classOf[Integer],classNameRenderer)
	ownPropTable.peer.setDefaultRenderer(classOf[Integer],classNameRenderer)	
	val childDefMod=new ChildDefTableModel	
	val childDefColMod=new FieldColumnModel{
    	createColumn(0,"EditorName",100)
    	createColumn(1,"ChildClass",100)
    	createColumn(2,"ActionName",110)    	
    }
	val childDefTable=new FieldTable(childDefMod,childDefColMod)
	childDefTable.peer.setDefaultRenderer(classOf[Integer],classNameRenderer)
	val childDefColor=childDefTable.background
	val disableModels=List(inheritedFieldMod,ownFieldMod,inheritedPropMod,ownPropMod,childDefMod)
	
	val classEditor=new ClassNameEditor(new JComboBox)
	ownPropTable.peer.setDefaultEditor(classOf[Integer],classEditor)
	childDefTable.peer.setDefaultEditor(classOf[Integer],classEditor)
	
	class FieldTable(mod:TableModel,cm:TableColumnModel=null) extends Table {
		val stringEditor=new MultilineEditor(peer)	{
			def setEditorValue(value:Object) = if(value==null) "" else value.toString
		}
		model=mod
		peer.setRowHeight(19)
		peer.setDefaultEditor(classOf[String],stringEditor)		
		peer.setDefaultEditor(classOf[DTWrap],typeEditor)	
		autoResizeMode=Table.AutoResizeMode.Off
		selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None    
    if(cm!=null) {
    	peer.setAutoCreateColumnsFromModel(false)
    	peer.setColumnModel(cm)
    }
    showGrid=true
		gridColor=Color.gray    
	}
	
	val formatStringLines=Array(
		new FormatLine(100,"Short Format",()=>theClass.shortFormat.toString,(s)=>theClass.shortFormat =InstFormat.fromString(s)),
		new FormatLine(100,"Long Format",()=>theClass.longFormat.toString,(s)=>theClass.longFormat =InstFormat.fromString(s)),
		new FormatLine(100,"Result Format",()=>theClass.resultFormat.toString,(s)=>theClass.resultFormat =InstFormat.fromString(s))	)
	val moduleLine=new FormatLine(100,"ActionModule",()=>theClass.moduleName,(s)=>theClass.moduleName=s)
	
	
	val basicsPan = new BoxPanel(Orientation.Horizontal ) {						
			idEdit.inputVerifier=checkID
			contents+=new Label("id:")+=idEdit+=Swing.HStrut(10)+=new Label("Name:")+=nameEdit+=Swing.HStrut(10)+=
				new Label("Description:")+=descriptionEdit
			maximumSize=new Dimension(Short.MaxValue,50)
		}
	
	val superClassesPan = new BoxPanel(Orientation.Horizontal) {	
		val overTopBorder = BorderFactory.createTitledBorder("Superclasses");
		overTopBorder.setTitlePosition(TitledBorder.ABOVE_TOP);
		border=overTopBorder
		val leftScroller=new ScrollPane {
			viewportView=classesListview
		}
		val centerPan= new BoxPanel(Orientation.Vertical){
			contents+=addSuperClassBut+=removeSuperClassBut+=Swing.VGlue
		}
		val rightScroller=new ScrollPane {
			viewportView=superClassesListview
		}		
		contents+=leftScroller+=centerPan+=rightScroller
		listenTo(addSuperClassBut,removeSuperClassBut)
		reactions += {
			case ButtonClicked(`addSuperClassBut`)=> if(classesListview.selection.indices.size==1) {
				 val selIx=classesListview.selection.indices.first
				 val newType=MainWindow.shortClassList (selIx)._1
				 if(!theClass.superClasses .contains(newType)){
					 theClass.superClasses=theClass.superClasses:+newType
					 updateInheritedFields
				 }
			}
			case ButtonClicked(`removeSuperClassBut`)=>if(superClassesListview.selection.indices.size==1)  {
				val selIx=superClassesListview.selection.indices.first
				val remType=theClass.superClasses(selIx)
				theClass.superClasses=theClass.superClasses.filterNot(_ ==remType)
				updateInheritedFields
			}
		}
	}
	
	val formatStringPan = new BoxPanel(Orientation.Vertical) {
		val overTopBorder = BorderFactory.createTitledBorder("Format Strings");
		overTopBorder.setTitlePosition(TitledBorder.ABOVE_TOP);
		border=overTopBorder
		contents ++=formatStringLines		
	}
	
	class FormatLine(labelWidth:Int,labelText:String,getter:()=> String,setter: (String)=>Unit) extends 
		BoxPanel(Orientation.Horizontal)
	{
		val label=new Label(labelText+":")
		label.preferredSize=new Dimension(labelWidth,0)
		val edit=new TextField(getter())
		contents+=label+=edit
		def update() = edit.text=getter()
		listenTo(edit)
		reactions+= {
			case e:EditDone => setter(edit.text)
		}	
		maximumSize=new Dimension(Short.MaxValue,30)
	}
	
	private def makeHeaderComp(table:Table):Component= new Component  {
			override lazy val peer=table.peer.getTableHeader
		} 	
		
	
	val fieldPan = new BorderPanel {
		val overTopBorder = BorderFactory.createTitledBorder("Fields");
		overTopBorder.setTitlePosition(TitledBorder.ABOVE_TOP);
		border=overTopBorder
		val headerComp= makeHeaderComp(ownFieldTable)		
		add(new UnWheelingScroller(){
			viewportView= new BoxPanel(Orientation.Vertical) {				
				contents+=headerComp+=inheritedFieldTable+=ownFieldTable				
			}			
		},BorderPanel.Position.Center)		
		inheritedFieldTable.background=Color.lightGray
		preferredSize=new Dimension(700,180)
	}
	
	
	val propFieldPan = new BoxPanel(Orientation.Horizontal) {
		val overTopBorder = BorderFactory.createTitledBorder("PropertyFields");
		overTopBorder.setTitlePosition(TitledBorder.ABOVE_TOP);
		border=overTopBorder
		contents+=new UnWheelingScroller(){			
			val header= makeHeaderComp(inheritedPropTable)
			viewportView= new BoxPanel(Orientation.Vertical) {
				contents+=header+=inheritedPropTable+=ownPropTable								
			}			
			preferredSize=new Dimension(290,100)
		}
		contents+=Swing.HStrut(10)+=new UnWheelingScroller(){			
			viewportView= childDefTable			
			preferredSize=new Dimension(310,100)
		}
		listenTo(inheritedPropTable.selection)
		listenTo(ownPropTable.selection)
		reactions+= {
					case TableRowsSelected(table,range,live)=> if (!live){						
						var row=table.peer.getSelectedRow
						if(row> -1){							
							val mod=table.model.asInstanceOf[PropFieldTableModel]
							println("select row:"+row+" size:"+mod.propFieldList .size)
							//row=row-inheritedPropMod.getRowCount
							if(row<mod.propFieldList.size){
								val propField=mod.getPropField(row)
								val ed=childDefTable.peer.getCellEditor()
								if (ed != null)  ed.stopCellEditing();  
								childDefMod.setValues(propField.createChildDefs,mod,row)
							}
							else childDefMod.setValues(Seq.empty,null,0)
							if (mod==ownPropMod){
								inheritedPropTable.peer.getSelectionModel.clearSelection
								childDefTable.background=childDefColor
							}
							else {
								ownPropTable.peer.getSelectionModel.clearSelection
								childDefTable.background=Color.lightGray
							}
						}
					}
				}
	}
	
	val actionPan=new BoxPanel(Orientation.Horizontal) {
		border=BorderFactory.createEmptyBorder(10,5,10,5)
		val checkBut=new Button("Check out")
		contents+=saveBut+=checkBut+=Swing.HGlue
		listenTo(saveBut,checkBut)
		reactions+= {
			case ButtonClicked(`saveBut`) => safeClass
			case ButtonClicked(`checkBut`) => {
				updateClassInfo
				println(theClass.saveToXML.mkString("\n"))
			}
		}
	}
	
	def updateClassInfo = disableModels.foreach(_.update(theClass))
	
	def safeClass = {
		updateClassInfo
		if(isCreating) {
			SessionManager.scl .classList=SessionManager.scl.classList+(theClass.id -> theClass)
			MainWindow.generateDataList
		}			
		scala.xml.XML.save(FSPaths.configDir+"types.xml",SessionManager.scl.saveToXML,"UTF-8",true,null)
		
	}
	
	
	val generalPan = new BoxPanel(Orientation.Vertical) {
		val belowTopBorder = BorderFactory.createTitledBorder("General Info");
		belowTopBorder.setTitlePosition(TitledBorder.BELOW_TOP);
		border=belowTopBorder
		contents+=basicsPan+=superClassesPan+=Swing.VStrut(10)+=moduleLine
	}
	
	def checkID (comp:Component):Boolean = {
		var n:Int= -1
		try { n=comp.asInstanceOf[TextField].text.toInt } catch {
			case e => false 
		}
		if (isCreating)! MainWindow.usedIDs.contains(n)
		else true
	} 
	
	
  viewportView= new BoxPanel(Orientation.Vertical){
		override lazy val peer = new javax.swing.JPanel with SuperMixin with javax.swing.Scrollable {       
			val l = new javax.swing.BoxLayout(this, Orientation.Vertical.id)
			setLayout(l)
			def getPreferredScrollableViewportSize: Dimension=getPreferredSize 
			def getScrollableTracksViewportHeight: Boolean =false
			def getScrollableTracksViewportWidth: Boolean=false
			def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = 200  
			def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int= 10  
		}
  	contents+=generalPan+=formatStringPan+=fieldPan+=propFieldPan+=actionPan  	
  }
  
  def setClass(newClass:ServerObjectClass,create:Boolean) = {
  	isCreating=create
  	disableComps.foreach(_.enabled=isCreating)
  	disableModels.foreach(_.isCreating=create)
		theClass=newClass
		nameEdit.text=theClass.name
		idEdit.text=theClass.id.toString
		descriptionEdit.text=theClass.description
		classesListview.listData=MainWindow.shortClassList		
		formatStringLines.foreach( _.update)		
		moduleLine.update
		setInheritedFields
	} 
  
  def setInheritedFields = {
  	superClassesListview.listData=theClass.superClasses.map(id => (id,AllClasses.get.getClassByID(id).name))
    val numOwnFields=theClass.getNumOwnFields
		val fieldSettingsList=theClass.getFieldSettingsList
		ownFieldMod.setValues(theClass.fields.view.takeRight(numOwnFields),
			fieldSettingsList.takeRight(numOwnFields))
		inheritedFieldMod.setValues(theClass.fields.view.dropRight(numOwnFields),
			fieldSettingsList.dropRight(numOwnFields))
		val numOwnPropFields=theClass.getNumOwnPropFields
		inheritedPropMod.setValues(theClass.propFields.view.dropRight(numOwnPropFields))
		ownPropMod.setValues(theClass.propFields.view.takeRight(numOwnPropFields))
		childDefMod.setValues(Seq.empty,null,0)	
  }
  
  def updateInheritedFields = {
  	theClass=theClass.makeClone
  	SessionManager.scl.addClass(theClass)
    setInheritedFields	
  }
  
  
  class UnWheelingScroller extends ScrollPane {
		val wheelListeners=peer.getMouseWheelListeners
			for(l <-wheelListeners) peer.removeMouseWheelListener(l)
			peer.setWheelScrollingEnabled(false)
	}	
  
  class ClassNameRenderer extends JLabel with TableCellRenderer {
  	override def invalidate = {}
  	override def revalidate = {}
  	def getTableCellRendererComponent(table:JTable, a:Object, isSelected: Boolean, focused: Boolean,  row: Int,col:Int):java.awt.Component = {
  		setText(if(a==null || !a.isInstanceOf[Integer])""
  		else {
  			val aValue=a.asInstanceOf[Integer].intValue
			  if(aValue<0) "None" 
			  else if(aValue==0)	"Any"
				else AllClasses.get.getClassByID(aValue).name
  		})
  		this
		}
	}
  
  class ClassNameEditor(box:JComboBox) extends DefaultCellEditor(box) with ClassListListener {
  	var classList:Seq[Int]=Seq.empty
  	var classNameList=Array[AnyRef]()
  	def classListChanged(list:Seq[(Int,String)])= {
  	   classList=0 +: list.map(_._1).toSeq
  	   classNameList="Any" +: list.map(_._2.asInstanceOf[AnyRef]).toArray
  	   box.setModel(new DefaultComboBoxModel(classNameList))
  	 } 	   	  	
  	MainWindow.registerClassListListener(this)
  	
  	override def getTableCellEditorComponent(table: JTable,value: Object,isSelected:Boolean,row:Int,column:Int ) = {
  		val editor:JComboBox =super.getTableCellEditorComponent( table, value, isSelected, row, column ).asInstanceOf[JComboBox]; 
  		editor.setSelectedIndex( classList.indexOf(value.asInstanceOf[Integer].intValue) );
  		editor
  	}
  	override def getCellEditorValue():java.lang.Object =
  	{
  		val ix =getComponent().asInstanceOf[JComboBox].getSelectedIndex();
  		println("getValue "+ix)
  		if(ix<0) new Integer(-1)
  		else classList(ix).asInstanceOf[AnyRef]
  	}
  }
  class TypeEditor(box:JComboBox) extends DefaultCellEditor(box) {  	
  	override def getTableCellEditorComponent(table: JTable,value: Object,isSelected:Boolean,row:Int,column:Int ) = {
  		println("get comp")
  		val editor:JComboBox =super.getTableCellEditorComponent( table, value, isSelected, row, column ).asInstanceOf[JComboBox]; 
  		editor.setSelectedItem( value )
  		editor
  	}
  	override def getCellEditorValue():java.lang.Object =
  	{
  		getComponent().asInstanceOf[JComboBox].getSelectedItem();  		
  	}
  }
}