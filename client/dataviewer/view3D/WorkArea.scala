/**
 * Author: Peter Started:22.05.2011
 */
package client.dataviewer.view3D
import client.comm.{ClientQueryManager}
import definition.comm.NotificationType
import definition.data.{Reference,InstanceData,Referencable}
import definition.typ.SystemSettings
import definition.expression.{VectorConstant, Line3D}
import javax.media.j3d.{Shape3D, LineStripArray, LineArray, GeometryArray, Font3D, Text3D,FontExtrusion,OrientedShape3D,
	Appearance,TransformGroup,Transform3D,LineAttributes,ColoringAttributes,BoundingBox}
import javax.vecmath.{Color3f, Point3d,Point3f,Vector3f,Vector3d}
import java.awt.Font

/**
 * 
 */
class WorkArea(val ref:Reference,val editor:BuildingEditor) extends Referencable {
	val lock=new Object
	var planeList:Seq[PlaneElement]=Seq.empty
	
	var prism:Option[BorderPrism]=None
	
	val prismListeners=new collection.mutable.HashSet[(Option[BorderPrism])=>Unit]()
	
	var planeSubsID:Int=0
	var instSubsID:Int=0
	var volSubsID:Int=0
	var instanceData:Option[InstanceData]=None
	
	var topVolume:Option[SubVolume]= None
	
	lazy val volumeType:Int=SystemSettings().systemTypes("TeilVolumen")
	
	val planeListeners=new collection.mutable.HashMap[Reference,WorkArea.planeUpdateFunc]()
	
		load()
	
	def load() = lock.synchronized{
		instSubsID= ClientQueryManager.createSubscription(ref,-1) {
			(typ:NotificationType.Value,data:IndexedSeq[InstanceData])=> ClientQueryManager.runInPool{  
				lock.synchronized {
					typ match {				
						case NotificationType.sendData|NotificationType.FieldChanged  =>{
						  instanceData=Some(data(0))
						}
					}
				}
			}
		}
		
		planeSubsID=ClientQueryManager .createFactSubscription[PlaneElement](ref,0.toByte,PlaneFactory){
			(typ:NotificationType.Value,data:IndexedSeq[PlaneElement])=> ClientQueryManager.runInPool{  
				lock.synchronized {
					typ match {
						case NotificationType.sendData =>{
							planeList=data
							planesLoaded()
						}

						case NotificationType.FieldChanged => {    					
							val searchRef=data(0).ref
							val ix=planeList.findIndexOf(_.ref == searchRef)
							if(ix<0) error("can't find changed Plane "+searchRef)
							else {    						
								planeList=planeList.updated(ix,data(0))
								if(planeListeners.contains(data(0).ref)) 
									planeListeners(data(0).ref)(data(0))
								planeUpdated()
							}
						}
						case NotificationType.instanceRemoved=> {
							planeList=Seq.empty
							planeUpdated()
						}
					}
				}
			}
		}	
		
	}
	
	
	
	def addPrismListener(ufunc:(Option[BorderPrism])=>Unit) ={ 
		prismListeners += ufunc
		if(prism.isDefined)ufunc(prism) // when the prism is already loaded, send the update now
	}
	
	
	
	def registerPlaneListener(planeRef:Reference, ufunc:WorkArea.planeUpdateFunc)= {
		val ix=planeList.indexWhere(_.ref==planeRef)
		if(ix > -1) {
			planeListeners(planeRef)=ufunc
			ufunc(planeList(ix))
		}
		else for(t <-topVolume)
			t.getPlaneHolder(planeRef) match {
			case Some(holder)=> holder.registerAxisPlaneListener(planeRef,ufunc)
			case _ => println("register plane listener cant find holder for Axis: "+planeRef)
		}
	}
	
	
	
	
	def planesLoaded()={
		// load the top Volume and thus all child volumes
		planeUpdated()
		volSubsID= ClientQueryManager.createSubscription(ref,2) {
			(typ:NotificationType.Value,data:IndexedSeq[InstanceData])=> ClientQueryManager.runInPool{  
				lock.synchronized {
					typ match {
						case NotificationType.sendData|NotificationType.FieldChanged =>{
							
							for(t <-topVolume) t.shutDown
							if(data.size==0){ // no volume yet
								//println("create new volume")
								SubVolume.createSubVolume(this, ref, 2, planeList)
							}
							else {
								//println("send volume "+data(0))
								topVolume=Some(new SubVolume(this,data(0),None))
								topVolume.get.loadData()
								volumeLoaded()
							}
							
						}
						case NotificationType.childAdded => {
							for(t <-topVolume) t.shutDown
							topVolume=Some(new SubVolume(this,data(0),None))
							topVolume.get.loadData()
							volumeLoaded()
						}
					}
				}
			}
		}		
		
	}
	
	
	def volumeLoaded() = {	
		 //println("Volume loaded:"+topVolume+" "+prism)
		for(t <-topVolume;p<-prism) {
			//println("Volume loaded:"+t.name+" ")
			p.addChild(t)
		}
			
	}
	
	//def allPlanesLoaded= ! holderList.exists(_.plane ==None)
	
	def arePlanesLoaded = lock.synchronized{planeList.size==6}
	
	def isAllowedGeometry = lock.synchronized{		
		!planeList(0).isVertical && 
		!planeList(1).isVertical &&
		!planeList(2).isHorizontal &&
		!planeList(3).isHorizontal &&
		!planeList(4).isHorizontal &&
		!planeList(5).isHorizontal 
	}
		
	
	def planeUpdated() = if(arePlanesLoaded) lock.synchronized{
		println("Plane updated: loaded:"+arePlanesLoaded+" allowed:"+isAllowedGeometry+" topVolume:"+topVolume+" prism:"+prism)
		val text=instanceData match {case Some(data)=> data.fieldValue(0).toString;case _ => ""}
		prism= if(isAllowedGeometry) {
			for(t<-topVolume;p<-prism;if(t.getParent!=null)) p.removeChild(t)
			val newPrism=new BorderPrism(ref,text,planeList)			
			for(t<-topVolume) {			
				newPrism.addChild(t)
			}
			Some( newPrism)
		}
		 else None	
		 
		prismListeners.foreach(_(prism)) 
	}
	
	
	
	def shutDown() = lock.synchronized {
		if(planeSubsID!=0) {
			ClientQueryManager.removeSubscription(planeSubsID)
			planeSubsID==0
		}
		if(instSubsID!=0){
			ClientQueryManager.removeSubscription(instSubsID)
			instSubsID==0
		}
		if(volSubsID!=0) {
			ClientQueryManager.removeSubscription(volSubsID)
			volSubsID==0
		}
		for(t<-topVolume) t.shutDown
	}
	
	
	def selectShape(shape:RefShape)= {
		for(t<-topVolume) t.selectShape(shape)
	}
	
	def deselectShape(shape:RefShape)= {
		for(t<-topVolume) t.deselectShape(shape)
	}
}

object WorkArea {
	type planeUpdateFunc=(PlaneElement)=> Unit
	/** checks if the given condition is true for all pair combinations of list elements
	 * 
	 * @param list a list with at least 2 elements
	 * @param func a condition to check for each 2 elements in this list
	 * @return
	 */
	def checkCombinated[A](list:List[A],func:(A,A)=>Boolean):Boolean = {
		val si=list.size
		if(si<2 )return false		
		if(si==2)return func(list.head,list.tail.head)
		for(tailEl <-list.tail)
		  if(!func(tailEl,list.head))return false
		checkCombinated(list.tail,func)	
	}
	
	def createAxisText(text:String,position:Point3d,font3d:Font3D,color:Color3f,scale:Float=1f):TransformGroup= {
	  val xTG = new TransformGroup()
	  val xTrans=new Transform3D()
	  val textGeom = new Text3D(font3d, text,new Point3f(),Text3D.ALIGN_CENTER,Text3D.PATH_RIGHT)
	  val bb=new BoundingBox
	  textGeom.getBoundingBox(bb)
	  val upper=new Point3d	  
	  bb.getUpper(upper)
	  var h=upper.y
	  bb.getLower(upper)
	  h-=upper.y
	  xTrans.set(new Vector3d(position.x,position.y,position.z-h))
	  xTrans.setScale(scale)
	  xTG.setTransform(xTrans)	  
	  val appear=new Appearance()
	  appear.setColoringAttributes(new ColoringAttributes(color,ColoringAttributes.NICEST))
	  
	  val textShape=new OrientedShape3D(textGeom,appear,OrientedShape3D.ROTATE_ABOUT_POINT,new Vector3f(0,0,0))
	  xTG.addChild(textShape)
	  xTG
	}
	
	
}


