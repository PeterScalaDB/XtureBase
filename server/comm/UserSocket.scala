/**
 * Author: Peter Started:29.08.2010
 */
package server.comm

import java.net._
import java.io._
import server.config._
import definition.comm._
import definition.typ.AllClasses
import definition.data._
import definition.expression._
import server.storage._
import scala.collection.mutable.HashMap
import transaction.handling._
import server.test.SimpleProfiler
import scala.Console

/** manages communications with a client
 * 
 */
class UserSocket(socket: Socket) extends Thread ("userSocket") {

	private val outStreamLock : AnyRef = new Object()
	private val out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream())); 
	private var userName=""	
	var userEntry:UserEntry=null	
	val socketLock:AnyRef = new Object
		
	type CommandHandlerFunc= (DataInputStream)=>Unit

	private var commandHandlerMap= scala.collection.mutable.HashMap[ClientCommands.Value,CommandHandlerFunc]()
	val queryHandler=new UserQueryHandler(this)
	
	// register routines
	registerCommandHandler(ClientCommands.writeField) (wantWriteField)
	registerCommandHandler(ClientCommands.createInstance)(wantCreateInstance)
	registerCommandHandler(ClientCommands.deleteInstance)(wantDeleteInstance)
	registerCommandHandler(ClientCommands.copyInstance)(wantCopyInstance)
		
	

	override def run ():Unit = { // receiving loop
			try {	

				val in = new DataInputStream( new BufferedInputStream( socket.getInputStream()));
				try
				{
					userName=in.readUTF()
					val passWord=in.readUTF()  	
					print("user "+userName+" logged in ")
					Thread.`yield`()
					if(!UserList.list.contains(userName)) {
						writeOut("User "+userName+" not known")
						return
					}
					val user= UserList.list(userName)					
					if(! (passWord==user.password)) {
						writeOut("Wrong password")
						return
					}

					if(ActiveUsers.isOnline(user.id)) {
						writeOut("User "+userName+" already online" )
						return
					} 
					writeOut("welcome")

					println("and added")
					userEntry=new UserEntry(user,this,queryHandler)
					ActiveUsers.addUser(userEntry)
					// start command loop
					handleCommands(in,user)	
				}
				finally {
					out.close()
					in.close()
				}
			}
			catch {
				case e: SocketException => println("Socket failed: "+e) 
				 // avoid stack trace when stopping a client with Ctrl-C
				case e: IOException =>
				e.printStackTrace();
			}
		}

		private def writeOut(st:String ) = {out.writeUTF(st);out.flush()}


		private def handleCommands(in:DataInputStream,user:UserInfo):Unit = {			
			var wantRun=true
			try {
				while(wantRun)
				{					
					val command =ClientCommands(in.readByte.toInt)
					println("#"+user.id+"# ClientCommand:"+command)
					try {
						command match {
							case ClientCommands.getTypes => sendTypes()
							case ClientCommands.logOut => {wantRun=false }
							case ClientCommands.getSetupData => sendSetupData()
							case ClientCommands.storeSetupData=> storeSetupData()
							case a => if (commandHandlerMap.contains(a))commandHandlerMap(a)(in)
							else println("unhandled command "+a)
						}			  
					}
					catch {
						case e:Exception => e.printStackTrace;
					}				
				}
			}
			finally {
				logoutUser(user.id)
			}
		}

		private def logoutUser(id:Int) = 	{
			println("user "+userName+" logged off")
			ActiveUsers.removeUser(id)
		}

		def sendData(command:ServerCommands.Value)(func:  (DataOutputStream)=>Unit) =
			socketLock.synchronized		{
			try {
				outStreamLock.synchronized {				
					out.writeByte(command.id.toByte)
					func(out)
					out.flush()
				}
			}
			catch {			
				case e: IOException =>
				e.printStackTrace();
			}
		}

		private def sendTypes() = 	{
			println("Sending Types to "+userName)
			sendData(ServerCommands.sendTypes) {out=>
				out.writeUTF(AllClasses.get.asInstanceOf[ServerClassList].toXML().toString())
			}
		}

		private def sendSetupData() = {

		}

		private def storeSetupData() = {
			println("store setup data")
		}
		
		def registerCommandHandler(command:ClientCommands.Value)(func:CommandHandlerFunc) = {
			commandHandlerMap.put(command,func)
		}
		
		private def wantWriteField(in:DataInputStream) = {
			var error:CommandError=null
			var result:Constant=null
			val ref=Reference(in)
			val field=in.readByte
			val expr=Expression.read(in)
			try {
				val ret=TransactionManager.doTransaction {
					if (!TransactionManager.tryWriteInstanceField(ref,field,expr))
						error=new CommandError("Unknown Issue",ClientCommands.writeField.id,0)
				}
				for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.copyInstance.id,0)
			} 
			catch {
				case e:Exception =>
				e.printStackTrace()
				error=new CommandError(e.toString,ClientCommands.writeField.id,0)
			}
			sendData(ServerCommands.sendCommandResponse ) {out =>
			println("sendCommandResponse writeField "+ref+" "+expr)
			 if(error!=null) {
				 out.writeBoolean(true)
				 error.write(out)	
			 }
			 else {
				 out.writeBoolean(false) // no errors
				 out.writeBoolean(false) // no result value
			 }
			}			 
		}
		
		
		private def wantCreateInstance(in:DataInputStream) = {
			var error:CommandError=null
			var result:Long = -1
			val typ=in.readInt
			val ownerCount=in.readInt
			val ownerArray:Array[OwnerReference]=(for (i <-0 until ownerCount) yield OwnerReference.read(in)).toArray
			
			try {
				val ret=TransactionManager.doTransaction {
					val inst= TransactionManager.tryCreateInstance(typ,ownerArray,true)
					if (inst==null)	error=new CommandError("Unknown Issue",ClientCommands.createInstance.id,0)
					else result=inst.ref.instance 
				}
				for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.createInstance.id,0)
			} 
			catch {
				case e:Exception =>
				e.printStackTrace()
				error=new CommandError(e.toString,ClientCommands.createInstance.id,0)
			}
			sendData(ServerCommands.sendCommandResponse ) {out =>
			println("sendCommandResponse create typ "+typ+" result:" +result)
			 if(error!=null) {
				 out.writeBoolean(true)
				 error.write(out)	
			 }
			 else {
				 out.writeBoolean(false) // no errors		
				 out.writeBoolean(true)
				 LongConstant(result).write(out) // the result value
			 }
			}			 
		}
		
		private def wantDeleteInstance(in:DataInputStream) = {
			var error:CommandError=null
			var result:Constant=null
			val ref=Reference(in)			
			try {
				val ret=TransactionManager.doTransaction {
					if (!TransactionManager.tryDeleteInstance(ref,None))
						error=new CommandError("Unknown Issue",ClientCommands.writeField.id,0)
				}
				for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.deleteInstance.id,0)
			} 
			catch {
				case e:Exception =>
				e.printStackTrace()
				error=new CommandError(e.toString,ClientCommands.deleteInstance.id,0)
			}
			sendData(ServerCommands.sendCommandResponse ) {out =>
			println("send delete ready")
			 if(error!=null) {
				 out.writeBoolean(true)
				 error.write(out)	
			 }
			 else {
				 out.writeBoolean(false) // no errors
				 out.writeBoolean(false) // no result value
			 }
			}			 
		}
		
		private def wantCopyInstance(in:DataInputStream) = {
			var error:CommandError=null
			var result:Constant=null
			val ref=Reference(in)		
			val fromOwner=OwnerReference.read(in)
			val toOwner=OwnerReference.read(in)
			var instID=0L
			//SimpleProfiler.startMeasure("start copy")
			try {
				val ret=TransactionManager.doTransaction {
					instID=TransactionManager.tryCopyInstance(ref,fromOwner,toOwner,true)
					if(instID<0)
						error=new CommandError("Unknown Issue",ClientCommands.copyInstance.id,0)
					//SimpleProfiler.measure("preparing ready")
				}
				for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.copyInstance.id,0)
			} 
			catch {
				case e:Exception =>
				e.printStackTrace()
				error=new CommandError(e.toString,ClientCommands.copyInstance.id,0)
			}
			sendData(ServerCommands.sendCommandResponse ) {out =>
			 if(error!=null) {
				 out.writeBoolean(true)
				 error.write(out)	
			 }
			 else {
				 out.writeBoolean(false) // no errors
				 out.writeBoolean(true)
				 LongConstant(instID).write(out)				  
			 }
			}			 
		}

		

		def tellToQuit() = {
			println("tell Quit to "+userName)
			sendData(ServerCommands.wantQuit ) {out =>}
		}
}

