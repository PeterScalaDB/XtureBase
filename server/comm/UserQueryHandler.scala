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
	userSocket.registerCommandHandler(ClientCommands.stopSubscription  )(stopSubscription)

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
		println("adding Subscription for "+parentRef+" field:"+propertyField)
		val subsID=CommonSubscriptionHandler.addSubscription(userSocket.userEntry,parentRef,propertyField)
		userSocket.sendData(ServerCommands.acceptSubscription ) { out=>
			out.writeInt(subsID)
			sendQueryData(out,parentRef,propertyField)
		}
	}
	
	private def stopSubscription(in:DataInputStream) = {		
		val subsID=in.readInt
		println("Stop Subscription "+subsID)
		CommonSubscriptionHandler.removeSubscription(subsID)
	}
	
	def notifyInstanceChanged(subs:SubscriptionInfo,data:InstanceData) = {
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			out.writeInt(subs.id )
			out.writeInt(NotificationType.FieldChanged.id)
			data.ref.write(out)
			data.write(out)
		}
	}	
	

	private def sendQueryData(out:DataOutputStream,parentRef:Reference,propertyField:Byte) = {
		if (propertyField<0) // only get the parent Instance
		{		
			try {
				val inst= StorageManager.getInstanceData(parentRef)				
				out.writeInt(1)
				parentRef.write(out)
				inst.write(out)				
			}
			catch {
				case e: Exception => println(e); out.writeInt(0)
			}

		} else
		{ // get the child instances of the property field
			try {			
				StorageManager.getInstanceProperties(parentRef) match 
				{
					case Some(props) => { 
						val childRefs:List[Reference]=props.propertyFields(propertyField).propertyList						
						// get all Data before starting to write
						val instList = (for(cRef <- childRefs) yield (cRef,StorageManager.getInstanceData(cRef))).toList						
						out.writeInt(instList.size)
						for((ref,inst) <-instList)
						{
							ref.write(out)
							inst.write(out)
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
	
	

	
}