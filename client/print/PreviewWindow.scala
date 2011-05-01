/**
 * Author: Peter Started:22.04.2011
 */
package client.print

import scala.swing._
import scala.swing.event._
import java.awt.{Toolkit,Color}
import definition.data.PageData
import javax.swing.BorderFactory
import javax.swing.border.BevelBorder
import definition.data.RenderContext
import definition.data.FontStyleList
import definition.data.FormDescription
import client.testUI.ViewTest
import definition.data.Reference
import java.awt.event.MouseAdapter
import java.awt.event.MouseWheelEvent

/**
 * 
 */
class PreviewWindow(w:Window,preDialog:NewOutdefDialog)  extends Dialog(w){
   title="Druckvorschau"
  	 
   var thePageable:APageable=_
   
   var archiveListModel=new ArchiveListModel()
   preferredSize=new Dimension(1000,800)  
   maximize
   var dotPitch:Double=0.25
   
   var currentPage:Int=_
   
   val closeBut=new Button("Abbruch")   
   val pageHeightBut=new Button("S-Höhe")
   val pageWidthBut=new Button("S-Breite")
   val twoPagesBut=new Button("2 Seiten")
   val zoomInBut=new Button("+")
   val zoomOutBut=new Button("-")
   val zoomEdit=new TextField("100%")
   zoomEdit.maximumSize=new Dimension(70,30)
   val pageSettingsBut=new Button("Einstellungen...")
   val printBut=new Button("Drucken...")
   
   val firstPageBut=new Button("<<")
   val prevPageBut=new Button("<")
   val nextPageBut=new Button(">")
   val lastPageBut=new Button(">>")
   val pageEdit=new TextField()
   pageEdit.preferredSize=new Dimension(50,40)
   val numPageLab=new Label("/ 0")
   pageEdit.maximumSize=new Dimension(50,30)
   
   val previewContext=new  RenderContext {
	   var masterContext:RenderContext= MyContext
  	 
	   def setMasterContext(nm:RenderContext)= {
	  	 masterContext=nm
	   }
	   var scale:Double=1
  	 
  	 var changedFontStyleList:FontStyleList= _
  	 def getScale:Double=scale
  	 def setScale(ns:Double)= {
  		 scale =ns  		 
  		 changedFontStyleList=new FontStyleList(masterContext.fontStyleList.list .map(a=> a.changeSize((scale*25.4f/72.0/dotPitch).toFloat) ))
  		 //println("setScale internList"+internFontStyleList.list .mkString(",")+"\n changedList:"+changedFontStyleList.list.mkString(","))
  	 }  	 
  	 
  	 def fontStyleList=changedFontStyleList
  	 
  	 override def toUnit(mm:Double):Float=(mm*scale/dotPitch).toFloat
   } 
   
   var pageViewer:PageViewer=new PageViewer(previewContext)
   
   
   val pageScroller=new ScrollPane (){
  	 viewportView=pageViewer
  	 //peer.setWheelScrollingEnabled(false)
  	 peer.addMouseWheelListener(new MouseAdapter(){
  		 override def mouseWheelMoved(e:MouseWheelEvent):Unit = {
  			 //println("Wheel:"+e)
  			 if(peer.getVerticalScrollBar.isVisible||peer.getHorizontalScrollBar.isVisible)return
  			 if(e.getWheelRotation>0) incPage
  			 else decPage
  		 }
  	 })
   }
   
   class GroupPanel(labelText:String,elements:Component*) extends BorderPanel {
  	 val topLab=new Label(labelText)
  	 topLab.horizontalAlignment=Alignment.Center
  	 topLab.peer.putClientProperty("JComponent.sizeVariant", "small");
	   topLab.peer.updateUI
	   val buttonPane=new BoxPanel(Orientation.Horizontal){
	  	 contents++=elements
	   }
	   add(topLab,BorderPanel.Position.North )
	   add(buttonPane,BorderPanel.Position.Center)	
	   border=BorderFactory.createEtchedBorder()//createBevelBorder(BevelBorder.RAISED)
	   maximumSize=new Dimension(80,100)
   }
   
   val chosePagePan=new GroupPanel("Seite(n) anzeigen",firstPageBut,prevPageBut,pageEdit,numPageLab,nextPageBut,lastPageBut)   
   val zoomPanel=new GroupPanel("Vergrößerung",pageHeightBut,pageWidthBut,twoPagesBut,Swing.HStrut(10),zoomInBut,zoomOutBut,zoomEdit,new Label("%"))
   val outputPanel=new GroupPanel("Ausgabe",pageSettingsBut,printBut,closeBut)
   
   val topPane=new BoxPanel(Orientation.Horizontal ) {
  	 contents+=chosePagePan+=Swing.HGlue+=zoomPanel+=Swing.HGlue+=outputPanel  		 
  	 //background=Color.white
   }
   
   val archiveList=new ListView {
  	 peer.setModel(archiveListModel)
  	 selection.intervalMode=ListView.IntervalMode.Single
  	 listenTo(selection)
  	 reactions += {
  		 case ListSelectionChanged(list,range,live) => 			
			if (!live&& !selection.indices.isEmpty&&archiveScroller.visible) {
				val ix= selection.indices.first
				setCurrentData(archiveListModel.archiveList(ix))
			}
  	 }
  	 renderer=new ListView.AbstractRenderer[ArchivePageable,ArchiveRenderer](new ArchiveRenderer){
  		 def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: ArchivePageable, index: Int) {
			component.config(isSelected,focused,a,index)
		}
		
	}
   }
   
   val archiveScroller:ScrollPane=new ScrollPane{
  	 viewportView=archiveList  	   	 
  	 preferredSize=new Dimension(200,40)
   }
   
   val mainPanel=new BorderPanel(){
  	 add(topPane,BorderPanel.Position.North )
  	 add(pageScroller,BorderPanel.Position.Center )
  	 add(archiveScroller,BorderPanel.Position.West)
   }
   
   listenTo(closeBut,pageHeightBut,pageWidthBut,twoPagesBut,zoomInBut,zoomOutBut,zoomEdit,pageSettingsBut,printBut,
  	 nextPageBut,prevPageBut,firstPageBut,lastPageBut,pageEdit)
   
   def decPage=if(currentPage>1)setCurrentPage(currentPage-1)
   def incPage=if(currentPage<thePageable.pagesList.size)setCurrentPage(currentPage+1)
  	 
   reactions += {
  	 case ButtonClicked(`closeBut`)=> close
  	 case ButtonClicked(`printBut`)=> printToOutput
  	 case ButtonClicked(`zoomInBut`)=> setScale(previewContext.scale*1.25)  	 
  	 case ButtonClicked(`zoomOutBut`)=>setScale(previewContext.scale/1.25) 	
  	 case ButtonClicked(`pageSettingsBut`)=>{
  		 close
  		 PrintQuestionHandler.outdefDialog.changeOutdef
  	 }
  	 case ButtonClicked(`nextPageBut`)=> incPage
  	 case ButtonClicked(`prevPageBut`)=> decPage
  	 case ButtonClicked(`firstPageBut`)=> setCurrentPage(1)
  	 case ButtonClicked(`lastPageBut`)=> setCurrentPage(thePageable.pagesList.size)
  	 case EditDone(`pageEdit`)=> {
  		 try {
  			 val pn=pageEdit.text.toInt
  			 if(pn>0&&pn<=thePageable.pagesList.size)setCurrentPage(pn)
  		 }catch {case e=> }
  	 }
  	 case ButtonClicked(`pageHeightBut`)=> setToPageHeight
  	 case ButtonClicked(`pageWidthBut`)=>  setToPageWidth
   
   }
   
   def setToPageHeight= {
  		 val vAmount=(if(pageScroller.peer.getHorizontalScrollBar.isVisible)pageScroller.peer.getHorizontalScrollBar.
  				 getSize().height+3 else 10)
  		 val scWidth=pageScroller.peer.getHorizontalScrollBar
  		 val siz=pageScroller.peer.getViewport.getSize()
  		 setScale((siz.height-vAmount).toFloat*dotPitch/thePageable.pageHeight)
  	 }
   
   def setToPageWidth = {
  		 val hAmount=(if(pageScroller.peer.getVerticalScrollBar.isVisible)pageScroller.peer.getVerticalScrollBar.
  				 getSize().width+10 else 16)
  		 val siz=pageScroller.peer.getViewport.getSize()
  		 setScale((siz.width-hAmount).toFloat*dotPitch/thePageable.pageWidth)
  	 }
   
   def printToOutput={
  	 //thePageable.context =MyContext
  	 val (pagesList,receiverList)=preDialog.printOutDialog.showPrintOutDialog(mainPanel,thePageable,currentPage)
  	 if(pagesList!=null){
  		 thePageable.tempContext=null   		  
  		 preDialog.pras.add(pagesList)
  		 preDialog.printJob.print(preDialog.pras)  	
  		 preDialog.storePrintData
  		 close
  	 }  	 
   }

   
   contents=mainPanel
   
   def setScale(nscale:Double)= {
  	 previewContext.setScale(nscale)
  	 zoomEdit.text="%,.2f".format(nscale*100)
  	 pageViewer.updateSize()
   }
   
   def showPreview(ntitle:String,nPageable:APageable)= {
  	 archiveScroller.visible=false
  	 archiveListModel.clear
  	 title="Druckvorschau "+ntitle
  	 visible=true
  	 setCurrentData(nPageable)    	 
  	 setCurrentPage(1)  	 
   }
   
   def showAll = {
  	 if (thePageable.pageWidth<thePageable.pageHeight) setToPageHeight else setToPageWidth
   }
   
   def showArchive(ntitle:String,outRef:Reference)= {
  	 archiveListModel.load(outRef)
  	 archiveScroller.visible=true  	 
  	 title=ntitle	   	 
  	 setCurrentPage(1)  	 
  	 if(archiveListModel.getSize>0)
  	   archiveList.selectIndices(0)  	   	
  	 numPageLab.text="/ "+thePageable.pagesList.size
  	 visible=true
   }
   
   def setCurrentData(nd:APageable):Unit = {  	  
  	 thePageable=nd
  	 thePageable.tempContext=null
  	 previewContext.setMasterContext(thePageable.context)
  	 thePageable.tempContext=previewContext
  	 pageViewer.setData(nd)
  	 numPageLab.text="/ "+thePageable.pagesList.size
  	 setCurrentPage(1)
  	 setScale(1)
  	 showAll
   }
   
   def maximize={
  	 val config = ViewTest.top.peer.getGraphicsConfiguration();
  	 val insets =Toolkit.getDefaultToolkit().getScreenInsets(config)
  	 val screenSize = Toolkit.getDefaultToolkit().getScreenSize();
  	 val w = screenSize.width/2// - insets.left - insets.right;
  	 val h = screenSize.height - insets.top - insets.bottom;
  	 //println("max: "+w+" "+h)
  	 preferredSize=new Dimension(w,h)
  	 peer.setBounds(insets.left+w/2,insets.top,w,h)  	 
   }
   
   def setCurrentPage(nr:Int)= {
  	 currentPage=nr
  	 pageEdit.text=nr.toString
  	 pageViewer.pageNr =nr
  	 pageViewer.repaint
   } 
   
}

