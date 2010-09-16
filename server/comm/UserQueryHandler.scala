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
	userSocket.registerCommandHandler(ClientCommands.pathSubs_openChild )(pathSubs_openChild)
	userSocket.registerCommandHandler(ClientCommands.pathSubs_jumpUp  )(pathSubs_jumpUp)
	userSocket.registerCommandHandler(ClientCommands.pathSubs_changePath  )(pathSubs_changePath)

	private def handleQuery(in:DataInputStream) = {
		val parentRef:Reference=Reference(in)
		val propertyField:Byte= in.readByte
		println("Processing Query for "+parentRef+" field:"+propertyField)
		userSocket.sendData(ServerCommands.sendQueryResponse ) {out=>
		sendQueryData(out,parentRef,propertyField)
		}		
	}
	
	private def newSubscription(in:DataInputStream) = {
		val parentRef:Reference=Reference(in)
		val propertyField:Byte= in.readByte		
		val subsID=CommonSubscriptionHandler.addSubscription(userSocket.userEntry,parentRef,propertyField)
		println("adding Subscription for "+parentRef+" field:"+propertyField+ " subsID:"+subsID)
		userSocket.sendData(ServerCommands.acceptSubscription ) { out=>
			out.writeInt(subsID)
			sendQueryData(out,parentRef,propertyField)
		}
	}
	
	
	private def changeSubscription(in:DataInputStream ) = {
		val subsID=in.readInt
		val newRef=Reference(in)
		val newPropField=in.readByte
		println("changing Subscription for subsID:"+subsID+ " to:"+newRef)
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
		println("adding Path Subscription for "+pathList.mkString("/")+" subsID:"+subsID)
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
				instList(i).write(out)							
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
		println("Stop Subscription "+subsID)
		CommonSubscriptionHandler.removeSubscription(subsID)
	}
	
	def notifyInstanceChanged(subs:SubscriptionInfo,data:InstanceData) = {
		//println("Notify changed "+subs)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			out.writeInt(subs.id )
			out.writeInt(NotificationType.FieldChanged.id)
			data.ref.write(out)
			data.write(out)
		}
	}	
	
	def notifyInstanceAdded(subs:SubscriptionInfo,data:InstanceData) = {
		//println("Notify added "+subs)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
		out.writeInt(subs.id)
		out.writeInt(NotificationType.childAdded.id )
		data.ref.write(out)
		data.write(out)
		}
	}
	
	def notifyInstanceDeleted(subs:SubscriptionInfo,ref:Reference) = {
		println("Notify deleted "+subs)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
		out.writeInt(subs.id)
		out.writeInt(NotificationType.instanceRemoved.id )
		ref.write(out)
		}
	}
	
	
	private def sendQueryData(out:DataOutputStream,parentRef:Reference,propertyField:Byte) = {
		if (propertyField<0) // only get the parent Instance				
			sendInstance(out,parentRef)
		else
		{ // get the child instances of the property field
			sendChildren(out,parentRef,propertyField)
		}	
	}
	
	private def sendInstance(out:DataOutputStream,ref:Reference) = 
	{
		try {
				val inst= StorageManager.getInstanceData(ref)				
				out.writeInt(1)
				ref.write(out)
				inst.write(out)				
			}
			catch {
				case e: Exception => println(e); out.writeInt(0)
			}
	}
	
	private def sendChildren(out:DataOutputStream,parentRef:Reference,propertyField:Byte) = 
	{
		try {			
				StorageManager.getInstanceProperties(parentRef) match 
				{
					case Some(props) => { 
						val childRefs=props.propertyFields(propertyField).propertyList						
						// get all Data before starting to write
						val instList = for(cRef <- childRefs) yield StorageManager.getInstanceData(cRef)						
						out.writeInt(childRefs.size)
						for(i <-childRefs.indices)
						{
							childRefs(i).write(out)
							instList(i).write(out)
						}					
					}
					case None => out.writeInt(0) 
				}
			}
			catch {
				case e: Exception => println(e); out.writeInt(0)
			}
	}
	
	

	
}