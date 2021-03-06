/**
 * Author: Peter Started:04.09.2010
 */
package server.comm

import definition.data._
import java.io._
import server.storage._
import definition.comm._

/** Handles Queries and Subscriptions of a certain user
 * 
 */
class UserQueryHandler(userSocket: UserSocket) {
	userSocket.registerCommandHandler(ClientCommands.queryInstance)( handleQuery)
	userSocket.registerCommandHandler(ClientCommands.startSubscription )(newSubscription)
	userSocket.registerCommandHandler(ClientCommands.changeSubscription )(changeSubscription)
	userSocket.registerCommandHandler(ClientCommands.startPathSubscription )(newPathSubscription)
	userSocket.registerCommandHandler(ClientCommands.stopSubscription  )(stopSubscription)
	userSocket.registerCommandHandler(ClientCommands.pauseSubscription  )(pauseSubscription)
	userSocket.registerCommandHandler(ClientCommands.pathSubs_openChild )(pathSubs_openChild)
	userSocket.registerCommandHandler(ClientCommands.pathSubs_jumpUp  )(pathSubs_jumpUp)
	userSocket.registerCommandHandler(ClientCommands.pathSubs_changePath  )(pathSubs_changePath)
	

	private def handleQuery(in:DataInputStream) = {
		val parentRef:Reference=Reference(in)
		val propertyField:Byte= in.readByte
		//System.out.println("Processing Query for "+parentRef+" field:"+propertyField)
		userSocket.sendData(ServerCommands.sendQueryResponse ) {out=>
		sendQueryData(out,parentRef,propertyField)
		}		
	}
	
	private def newSubscription(in:DataInputStream) = {
		val parentRef:Reference=Reference(in)
		val propertyField:Byte= in.readByte		
		val subsID=CommonSubscriptionHandler.addSubscription(userSocket.userEntry,parentRef,propertyField)
		//System.out.println("adding Subscription for "+parentRef+" field:"+propertyField+ " subsID:"+subsID)
		userSocket.sendData(ServerCommands.acceptSubscription ) { out=>
			out.writeInt(subsID)
			sendQueryData(out,parentRef,propertyField)
		}
	}
	
	def refreshSubscription(subs:SubscriptionInfo) = {
		//System.out.println("refreshing subscription "+subs)
		subs match {			
			case e:PropSubscription=> userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
				out.writeInt(subs.id )
				out.writeInt(NotificationType.sendData .id)
				sendQueryData(out,subs.parentRef,e.propertyField)
			}	
			case e:PathSubscription=> userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			  out.writeInt(subs.id )
			  out.writeInt(NotificationType.sendData .id)			  
				writePathElements(out,subs.id,e.path)
		  }
			case e:SingleSubscription=> userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
				out.writeInt(subs.id )
				out.writeInt(NotificationType.sendData .id)
				sendQueryData(out,subs.parentRef,-1)
			}
		}
	}
	
	
	private def changeSubscription(in:DataInputStream ) = {
		val subsID=in.readInt
		val newRef=Reference(in)
		val newPropField=in.readByte
		//System.out.println("changing Subscription for subsID:"+subsID+ " to:"+newRef)
		CommonSubscriptionHandler.changeSubscription(userSocket.userEntry,subsID,newRef,newPropField)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			out.writeInt(subsID )
			out.writeInt(NotificationType.sendData .id)
			sendQueryData(out,newRef,newPropField)
		}		
	}
	
	
	private def newPathSubscription(in:DataInputStream) = {
		val count=in.readInt
		val pathList=for(i <-0 until count) yield Reference(in)		
		val subsID=CommonSubscriptionHandler.addPathSubscription(userSocket.userEntry,pathList)
		//System.out.println("adding Path Subscription for "+pathList.mkString("/")+" subsID:"+subsID)
		userSocket.sendData(ServerCommands.acceptSubscription ) { out =>
			out.writeInt(subsID )
	  	writePathElements(out,subsID,pathList)						
		}
	}
	
	private def pathSubs_openChild(in:DataInputStream) = {
		val subsID=in.readInt
		val newRef=Reference(in)
		val list=CommonSubscriptionHandler.openChild(subsID,newRef)
		if(list!=null)
			userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			  out.writeInt(subsID )
			  out.writeInt(NotificationType.sendData .id)
			  //System.out.println("pathsubs openchild subsid:"+subsID+" "+list.mkString)
				writePathElements(out,subsID,list)
		  }
	}
	
	
	private def writePathElements(out:DataOutput,subsID:Int,refList:IndexedSeq[Reference]) = {
		
		try {
			// check if all instances are there
			val instList = for(cRef <- refList) yield StorageManager.getInstanceData(cRef)
			// write data
			out.writeInt(refList.size)
			for(i <- refList.indices){
				refList(i).write(out)
				instList(i).writeWithChildInfo(out)							
			}
		} catch {
			case e: Exception => e.printStackTrace;out.writeInt(0)
		}		
	}
		
	
	private def pathSubs_jumpUp(in:DataInputStream) = {
		val subsID=in.readInt
		val newPos=in.readInt
		CommonSubscriptionHandler.jumpUp(subsID,newPos)
	}
	
	private def pathSubs_changePath(in:DataInputStream) = {
		val subsID=in.readInt
		val count=in.readInt
		val pathList=for(i <-0 until count) yield Reference(in)
		CommonSubscriptionHandler.changePath(subsID,pathList)
	}
	
	
	
	private def stopSubscription(in:DataInputStream) = {		
		val subsID=in.readInt
		//System.out.println("Stop Subscription "+subsID)
		CommonSubscriptionHandler.removeSubscription(subsID)
	}
	
	private def pauseSubscription(in:DataInputStream) = {		
		val subsID=in.readInt
		//System.out.println("pause Subscription "+subsID)
		CommonSubscriptionHandler.pauseSubscription(subsID)
	}
	
	
	
	
	def notifyInstanceChanged(subs:SubscriptionInfo,data:InstanceData) = {
		System.out.println("Notify instance changed "+subs+" changedInst:"+data.ref+" "+data)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			out.writeInt(subs.id )
			out.writeInt(NotificationType.FieldChanged.id)
			data.ref.write(out)
			data.writeWithChildInfo(out)
		}
	}	
	
	/*def burstNotifyChanges(subs:SubscriptionInfo,parentRef:Reference,propField:Byte) = {
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			out.writeInt(subs.id )
			out.writeInt(NotificationType.sendData.id)
			sendQueryData(out,parentRef,propField)
		}
	}*/
	
	def notifyInstanceAdded(subs:SubscriptionInfo,data:InstanceData) = {
		System.out.println("Notify instance added "+subs+" "+data.ref+" "+data)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
		out.writeInt(subs.id)
		out.writeInt(NotificationType.childAdded.id )
		//out.writeInt(atPos)
		data.ref.write(out)
		data.writeWithChildInfo(out)
		}
	}
	
	def notifyInstanceDeleted(subs:SubscriptionInfo,ref:Reference) = {
		//System.out.println("Notify deleted "+subs+" "+ref)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
		out.writeInt(subs.id)
		out.writeInt(NotificationType.instanceRemoved.id )
		ref.write(out)
		}
	}
	
	
	private def sendQueryData(out:DataOutputStream,parentRef:Reference,propertyField:Byte) = {
		//System.out.println("sendQueryData:"+parentRef)
		if (propertyField<0) // only get the parent Instance				
			sendInstance(out,parentRef)
		else
		{ // get the child instances of the property field
			sendChildren(out,parentRef,propertyField)
		}	
		//System.out.println("sendQueryData finish")
	}
	
	private def sendInstance(out:DataOutputStream,ref:Reference) = 
	{
		try {
				val inst= StorageManager.getInstanceData(ref)				
				out.writeInt(1)
				ref.write(out)
				inst.writeWithChildInfo(out)				
			}
			catch {
				case e: Exception =>System.err.println("Error sending Instance "+ref); e.printStackTrace(); out.writeInt(0)
			}
	}
	
	private def sendChildren(out:DataOutputStream,parentRef:Reference,propertyField:Byte) = 
	{
		try {			
				StorageManager.getInstanceProperties(parentRef) match 
				{
					case Some(props) => { 
						val childRefs=props.propertyFields(propertyField).propertyList	
						//System.out.println("send Children:"+childRefs.map(_.sToString+","))
						// get all Data before starting to write						
						if(childRefs.size<10)
						{							
						  val instList= getInstances(childRefs)
						  out.writeInt(childRefs.size) // only write size after all children are found
						  //System.out.println("readlist "+instList)
						  for(i <-childRefs.indices)
						  {
						  	childRefs(i).write(out)
						  	instList(i).writeWithChildInfo(out)
						  }	
						}
						else {
							out.writeInt(childRefs.size) // ab bit risky if one child does not exist
							pushInstances(childRefs,out)					
						}
					}
					case None => out.writeInt(0) 
				}
			}
			catch {
				case e: Exception => System.err.println(e);e.printStackTrace; out.writeInt(0)
			}
	}
	
	private def pushInstances(childRefs:IndexedSeq[Reference],out:DataOutput) = {
		System.out.println("bulk push" +childRefs.head + " num:"+childRefs.size)
		var bulkStart= 0
  	var oldRef=childRefs.head
  	for(i <-1 until childRefs.size) {
  		val newRef=childRefs(i)
  		if(oldRef.typ== newRef.typ && oldRef.instance ==(newRef.instance-1)){
  				// subsequent instance
  			}
  		else { // another instance, break bulk block
  			if(bulkStart==i-1 )  // only single instance
  				StorageManager.pushInstanceData(oldRef,out)  				
  				else  StorageManager.bulkPushInstanceData(childRefs(bulkStart),oldRef,out)	  				
  				bulkStart=i
  			}
  			oldRef=newRef
  		}	
  		if(bulkStart==childRefs.size-1 ) { // only single instance
  			StorageManager.pushInstanceData(oldRef,out)
  		}
  		else
  			 StorageManager.bulkPushInstanceData(childRefs(bulkStart),oldRef,out)
  	 //System.out.println("push ready")		  			
  		
	}
	
	
	
  private def getInstances(childRefs:IndexedSeq[Reference]):Seq[InstanceData] = {
  	if(childRefs.size>10) {  
  		var retList=new collection.mutable.ArrayBuffer[InstanceData]()
  		var bulkStart= 0
  		var oldRef=childRefs.head
  		for(i <-1 until childRefs.size) {
  			val newRef=childRefs(i)
  			if(oldRef.typ== newRef.typ && oldRef.instance ==(newRef.instance-1)){
  				// subsequent instance
  			}
  			else { // another instance, break bulk block
  				if(bulkStart==i-1 )  // only single instance
  					retList += StorageManager.getInstanceData(oldRef)  				
  				else retList ++= StorageManager.bulkGetInstanceData(childRefs(bulkStart),oldRef)	  				
  				bulkStart=i
  			}
  			oldRef=newRef
  		}	
  		if(bulkStart==childRefs.size-1 ) { // only single instance
  			retList += StorageManager.getInstanceData(oldRef)
  		}
  		else {
  			val bulkList= StorageManager.bulkGetInstanceData(childRefs(bulkStart),oldRef)
  			if(childRefs.size==bulkList.size) {System.out.println("powerbulk");return  bulkList}
  			else {
  				//System.out.println(" sizediff childrefs:"+childRefs.size+" bulkList:"+ bulkList.size )
  				retList ++= bulkList
  			}  			
  		}
  		retList
  	}	
  	else if(childRefs.size>1) for(cRef <- childRefs) yield StorageManager.getInstanceData(cRef)
  	else if(!childRefs.isEmpty)IndexedSeq(StorageManager.getInstanceData(childRefs.head))
  	else IndexedSeq()
  }
	 
}