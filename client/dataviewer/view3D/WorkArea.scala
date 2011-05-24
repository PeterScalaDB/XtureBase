/**
 * Author: Peter Started:22.05.2011
 */
package client.dataviewer.view3D
import definition.data.Reference
import client.comm.ClientQueryManager
import definition.comm.NotificationType
import definition.expression.VectorConstant

/**
 * 
 */
class WorkArea(parentRef:Reference) {
	val lock=new Object
	var holderList:Seq[PlaneElement]=Seq.empty
	
	var prism:Option[BorderPrism]=None
	
	val prismListeners=new collection.mutable.HashSet[(Option[BorderPrism])=>Unit]()
	
	var subsID:Int=0
	
	load()
	
	def load() = lock.synchronized{
		subsID=ClientQueryManager .createFactSubscription[PlaneElement](parentRef,0.toByte,PlaneFactory){
			(typ:NotificationType.Value,data:IndexedSeq[PlaneElement])=> ClientQueryManager.runInPool{  
				lock.synchronized {
					typ match {
						case NotificationType.sendData =>{
							holderList=data
							planeUpdated()
						}

						case NotificationType.FieldChanged => {    					
							val searchRef=data(0).ref
							val ix=holderList.findIndexOf(_.ref == searchRef)
							if(ix<0) error("can't find changed Plane "+searchRef)
							else {    						
								holderList.updated(ix,data(0))
								planeUpdated()
							}
						}
						case NotificationType.instanceRemoved=> {
							holderList=Seq.empty
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
	
	
	//def allPlanesLoaded= ! holderList.exists(_.plane ==None)
	
	def arePlanesLoaded = lock.synchronized{holderList.size==6}
	
	def isAllowedGeometry = lock.synchronized{		
		!holderList(0).isVertical && 
		!holderList(1).isVertical &&
		!holderList(2).isHorizontal &&
		!holderList(3).isHorizontal &&
		!holderList(4).isHorizontal &&
		!holderList(5).isHorizontal 
	}
		
	
	def planeUpdated() = if(arePlanesLoaded) lock.synchronized{
		println("Plane updated: loaded:"+arePlanesLoaded+" allowed:"+isAllowedGeometry)
		prism= if(isAllowedGeometry) Some( new BorderPrism(holderList))
		 else None	
		prismListeners.foreach(_(prism)) 
	}
	
	
	
	def shutDown() = lock.synchronized {if(subsID!=0) ClientQueryManager.removeSubscription(subsID)}		
}

object WorkArea {
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
}

class BorderPrism(planeList:Seq[PlaneElement]) {
	val topPlane=planeList(0)
	val bottomPlane=planeList(1)	
	val northPlane=planeList(2)
	val eastPlane=planeList(3)
	val southPlane=planeList(4)
	val westPlane=planeList(5)
	val northUpEdge=topPlane.intersectionWith(northPlane)
	val southUpEdge=topPlane.intersectionWith(southPlane)
	val northLowEdge=bottomPlane.intersectionWith(northPlane)
	val southLowEdge=bottomPlane.intersectionWith(southPlane)
	val topCorners=IndexedSeq(westPlane.intersectionWith(northUpEdge),
														eastPlane.intersectionWith(northUpEdge),
														eastPlane.intersectionWith(southUpEdge),
														westPlane.intersectionWith(southUpEdge))
  val bottomCorners=IndexedSeq(westPlane.intersectionWith(northLowEdge),
														eastPlane.intersectionWith(northLowEdge),
														eastPlane.intersectionWith(southLowEdge),
														westPlane.intersectionWith(southLowEdge))	
														
	override def toString= {
		"BorderPrism \ntopCorners"+topCorners.mkString(",")+"\nbottomCorners:"+bottomCorners.mkString(",")
	}
}

