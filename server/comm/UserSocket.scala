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
import server.storage._
import scala.collection.mutable.HashMap

/** manages communications with a client
 * 
 */
class UserSocket(socket: Socket) extends Thread () {

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
				case e: SocketException =>
				() // avoid stack trace when stopping a client with Ctrl-C
				case e: IOException =>
				e.printStackTrace();
			}
		}

		private def writeOut(st:String ) = {out.writeUTF(st);out.flush()}


		private def handleCommands(in:DataInputStream,user:UserInfo):Unit = {			
			while(true)
			{					
				val command =ClientCommands(in.readByte.toInt)
				try {
					command match {
						case ClientCommands.getTypes => sendTypes()
						case ClientCommands.logOut => {logoutUser(user.id);return }
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
				out.writeUTF(AllClasses.toXML().toString())
			}
		}

		private def sendSetupData() = {

		}

		private def storeSetupData() = {

		}
		
		def registerCommandHandler(command:ClientCommands.Value)(func:CommandHandlerFunc) = {
			commandHandlerMap.put(command,func)
		}
		
		private def wantWriteField(in:DataInputStream) = {
			
		}

		

		def tellToQuit() = {
			println("tell Quit to "+userName)
			sendData(ServerCommands.wantQuit ) {out =>}
		}

}