/**
 * Author: Peter Started:29.08.2010
 */
package server.comm

import java.net._
import java.io._
import server.config._
import definition.comm._
import definition.typ.{AllClasses,ParamQuestion,SystemSettings}
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
	var currentEnquiryContinuation:(UserSocket,Seq[(String,Constant)])=>Unit = _

	// register routines
	registerCommandHandler(ClientCommands.writeField) (wantWriteField)
	registerCommandHandler(ClientCommands.writeMultiFields) (wantWriteMultiFields)
	registerCommandHandler(ClientCommands.createInstance)(wantCreateInstance)
	registerCommandHandler(ClientCommands.deleteInstance)(wantDeleteInstance)
	registerCommandHandler(ClientCommands.copyInstances)(wantCopyInstances)
	registerCommandHandler(ClientCommands.moveInstances)(wantMoveInstances)
	registerCommandHandler(ClientCommands.executeAction  )(executeAction(false))	
	registerCommandHandler(ClientCommands.executeCreateAction  )(executeAction(true))
	registerCommandHandler(ClientCommands.getUserSettings  )(getUserSettings)
	registerCommandHandler(ClientCommands.writeUserSettings  )(writeUserSettings)
	registerCommandHandler(ClientCommands.requestUndoData  )(requestUndoData)
	registerCommandHandler(ClientCommands.undo  )(doUndo)
	registerCommandHandler(ClientCommands.stopUndo  )(stopUndo)
	registerCommandHandler(ClientCommands.secondUseInstances )(create2ndUseCopies)
	registerCommandHandler(ClientCommands.answerEnquiry  )(answerEnquiry)


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

				System.out.println("and added")
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
			case e: SocketException => System.out.println("Socket failed: "+e) 
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
				//System.out.println("#"+user.id+"# ClientCommand:"+command)
				try {
					command match {
						case ClientCommands.getTypes => sendTypes()
						case ClientCommands.logOut => {wantRun=false }
						//case ClientCommands.getSetupData => sendSetupData()
						case ClientCommands.storeSetupData=> storeSetupData()
						case a => if (commandHandlerMap.contains(a))commandHandlerMap(a)(in)
						else System.err.println("unhandled command "+a)
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
		System.out.println("user "+userName+" logged off")
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
		System.out.println("Sending Types to "+userName)
		sendData(ServerCommands.sendTypes) {out=>
		out.writeUTF(AllClasses.get.asInstanceOf[ServerClassList].toXML().toString())
		SystemSettings().asInstanceOf[ServerSystemSettings].write(out)
		}
	}

	/*private def sendSetupData() = {

	}
	*/

	private def storeSetupData() = {
		System.out.println("store setup data")
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
			val ret=TransactionManager.doTransaction(userEntry.info.id,ClientCommands.writeField.id.toShort,
				ref,false,0,{
				if (!TransactionManager.tryWriteInstanceField(ref,field,expr))
					error=new CommandError("Unknown Issue",ClientCommands.writeField.id,0)
			})
			for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.writeField.id,0)
		} 
	catch {
		case e:Exception =>
		e.printStackTrace()
		error=new CommandError(e.toString,ClientCommands.writeField.id,0)
	}
	sendData(ServerCommands.sendCommandResponse ) {out =>
	//System.out.println("sendCommandResponse writeField "+ref+" "+expr)
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
	
	private def wantWriteMultiFields(in:DataInputStream) = {
		var error:CommandError=null
		var result:Constant=null
		val numInst=in.readInt
		val refList=for(i <-0 until numInst) yield Reference(in)
		val field=in.readByte
		val expr=Expression.read(in)
		try {
			val ret=TransactionManager.doTransaction(userEntry.info.id,ClientCommands.writeMultiFields.id.toShort,
				refList.head,true,0,{
				for(ref<-refList)
				if (!TransactionManager.tryWriteInstanceField(ref,field,expr))
					error=new CommandError("Unknown Issue",ClientCommands.writeField.id,0)
			})
			for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.writeMultiFields.id,0)
		} 
	catch {
		case e:Exception =>
		e.printStackTrace()
		error=new CommandError(e.toString,ClientCommands.writeField.id,0)
	}
	sendData(ServerCommands.sendCommandResponse ) {out =>
	//System.out.println("sendCommandResponse writeFields "+refList.size+" "+expr)
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
		var result:Int = -1
		val typ=in.readInt
		val ownerCount=in.readInt
		val ownerArray:Array[OwnerReference]=(for (i <-0 until ownerCount) yield OwnerReference.read(in)).toArray

		try {
			val ret=TransactionManager.doTransaction(userEntry.info.id,ClientCommands.createInstance.id.toShort,
				ownerArray.head.ownerRef ,false,typ,{
				val inst= TransactionManager.tryCreateInstance(typ,ownerArray,true)
				if (inst==null)	error=new CommandError("Unknown Issue",ClientCommands.createInstance.id,0)
				else result=inst.ref.instance
				//TransactionManager.currentRef=inst.ref // HACK to set the transDetailLog correctly
			})
			for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.createInstance.id,0)
		} 
	catch {
		case e:Exception =>
		e.printStackTrace()
		error=new CommandError(e.toString,ClientCommands.createInstance.id,0)
	}
	sendData(ServerCommands.sendCommandResponse ) {out =>
	//System.out.println("sendCommandResponse create typ "+typ+" result:" +result)
	if(error!=null) {
		out.writeBoolean(true)
		error.write(out)	
	}
	else {
		out.writeBoolean(false) // no errors		
		out.writeBoolean(true)
		IntConstant(result).write(out) // the result value
	}
	}			 
	}

	private def wantDeleteInstance(in:DataInputStream) = {
		var error:CommandError=null
		var result:Constant=null
		val ref=Reference(in)		
		val fromOwner=OwnerReference.read(in)
		try {
			val ret=TransactionManager.doTransaction (userEntry.info.id,ClientCommands.deleteInstance.id.toShort,
				ref,false,0,{
				if (!TransactionManager.tryDeleteInstance(ref,Some(fromOwner),None))
					error=new CommandError("Unknown Issue",ClientCommands.writeField.id,0)
			})
			for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.deleteInstance.id,0)
		} 
	catch {
		case e:Exception =>
		e.printStackTrace()
		error=new CommandError(e.toString,ClientCommands.deleteInstance.id,0)
	}
	sendData(ServerCommands.sendCommandResponse ) {out =>
	//System.out.println("send delete ready")
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

	private def wantCopyInstances(in:DataInputStream) = {
		copyOrMove(in,ClientCommands.copyInstances,false)
	}
	
	private def wantMoveInstances(in:DataInputStream) = {
		copyOrMove(in,ClientCommands.moveInstances,true)
	}
	
	private def copyOrMove(in:DataInput,command:ClientCommands.Value,move:Boolean)= {		
		var error:CommandError=null
		var result:Constant=null
		val numInstances=in.readShort
		val refList:Seq[Reference] = for(i <- 0 until numInstances) yield Reference(in)		
		val fromOwner:OwnerReference=OwnerReference.read(in)
		val toOwner:OwnerReference=OwnerReference.read(in)
		val atPos:Int = in.readInt
		var instID=0
		//SimpleProfiler.startMeasure("start copy")
		try {
			val ret=TransactionManager.doTransaction(userEntry.info.id,command.id.toShort,
				refList.first,numInstances>1,0,{
				if(move)TransactionManager.tryMoveMultiInstances(refList,fromOwner,toOwner,atPos)
				else {
					instID=TransactionManager.tryCopyMultiInstances(refList,fromOwner,toOwner,atPos)
					if(instID<0) error=new CommandError("Unknown Issue",command.id,0)
				}
				//System.out.println("actionList:"+ActionList.theList.mkString(" |"))
			})
			for(transError <-ret) error=new CommandError(transError.getMessage,command.id,0)
		} 
		catch {
			case e:Exception =>
			e.printStackTrace()
			error=new CommandError(e.toString,command.id,0)
		}
		sendData(ServerCommands.sendCommandResponse ) {out =>
			if(error!=null) {
				out.writeBoolean(true)
				error.write(out)	
			}
			else {
				out.writeBoolean(false) // no errors
				out.writeBoolean(true)
				IntConstant(instID).write(out)				  
			}
		}			 
	}
	
	private def create2ndUseCopies(in:DataInput)= {
		var error:CommandError=null
		var result:Constant=null
		val numInstances=in.readShort
		val refList:Seq[Reference] = for(i <- 0 until numInstances) yield Reference(in)		
		val fromOwner:OwnerReference=OwnerReference.read(in)
		val toOwner:OwnerReference=OwnerReference.read(in)
		val atPos:Int = in.readInt
		try {
			val ret=TransactionManager.doTransaction(userEntry.info.id,ClientCommands.secondUseInstances.id.toShort,
				refList.first,numInstances>1,0,{
				TransactionManager.trySecondUseInstances(refList,fromOwner,toOwner,atPos)  
			})
			for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.secondUseInstances.id,0)
		}
		catch {
			case e:Exception =>
			e.printStackTrace()
			error=new CommandError(e.toString,ClientCommands.secondUseInstances .id,0)
		}
		sendData(ServerCommands.sendCommandResponse ) {out =>
			if(error!=null) {
				out.writeBoolean(true)
				error.write(out)	
			}
			else {
				out.writeBoolean(false) // no errors
				out.writeBoolean(false) // no results								  
			}
		}		
	}
	
	

	private def executeAction(createAction:Boolean)(in:DataInputStream) = {
		var error:CommandError=null	
		val numInstances=in.readInt
		val instList=for(i <-0 until numInstances) yield StorageManager.getInstanceData(Reference(in))
		val newType=if(createAction)in.readInt else 0
		val propField=if(createAction)in.readByte else 0.toByte
		val actionName=in.readUTF
		val numParams=in.readInt
		val paramList=for(i <-0 until numParams) 
					yield (in.readUTF,Expression.readConstant(in))
		val owner=if(createAction)null else OwnerReference.read(in)
		try {
			val ret=TransactionManager.doTransaction(userEntry.info.id,ActionNameMap.getActionID(actionName),
				instList.head.ref,instList.size>1,newType,{
				//System.out.println("Execute Action "+actionName+ " instances:"+instList.mkString(",")+" create:"+createAction+" new Type:"+newType)
		    //System.out.println("params: "+paramList.mkString(","))
				if(actionName=="*" && createAction&&instList.size==1) simplyCreateInstance(instList,newType,propField)
				else {
					val theAction= if(createAction){					
						AllClasses.get.getClassByID(newType).createActions(actionName )
					}
					else AllClasses.get.getClassByID(instList.head.ref.typ).actions(actionName )
					theAction match {
						case a:ActionImpl => // simple action, order of execution is not important
						for ((typ,partList) <- instList.groupBy(_.ref.typ))
						{
							val theAction= AllClasses.get.getClassByID(typ).actions(actionName).asInstanceOf[ActionImpl]
							partList.foreach(a => theAction.func(this,a,paramList))
						}
						
						case b:ActionIterator => // Iterator, runs through all instances in given order 
						b.func(this,owner,instList,paramList)
						
						case c:CreateActionImpl if( createAction) => c.func(this,instList,paramList,newType)	
						case e => System.err.println("unknown type "+e+" "+createAction)
					}		
				}		
			})
			for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.executeAction.id,0)
		} 
	catch {
		case e:Exception =>
		e.printStackTrace()
		error=new CommandError(e.toString,ClientCommands.executeAction.id,0)
		case e:Error => e.printStackTrace()
		error=new CommandError(e.toString,ClientCommands.executeAction.id,0)
	}
	sendData(ServerCommands.sendCommandResponse ) {out =>
		if(error!=null) {
			out.writeBoolean(true)
			error.write(out)	
		}
		else {
			out.writeBoolean(false) // no errors
			out.writeBoolean(false) // no result						  
		}
	}			 
	}
	
	def simplyCreateInstance(parentList:Seq[InstanceData],newType:Int,propField:Byte) = {
		val ownerRef=new OwnerReference(propField,parentList(0).ref)
		val inst=TransactionManager.tryCreateInstance(newType,Array(ownerRef),true)		
	}
	
	def getUserSettings(in:DataInputStream) = {
		val file=new File(FSPaths.configDir+userName+".set")
		sendData(ServerCommands.sendUserSettings ) {out =>
			if(!file.exists) out.writeInt(0)
			else if(file.length==0) out.writeInt(0)
			else {
				val in=new FileInputStream(file)
				val l=file.length.toInt
				//System.out.println("read Settings: "+l)
				val readBuffer= new Array[Byte](l)
				in.read(readBuffer,0,l)
				out.writeInt(l)
				out.write(readBuffer,0,l)
				in.close				
			}			
		}
	}
	
	def writeUserSettings(in:DataInputStream) = {		
		val file=new File(FSPaths.configDir+userName+".set")
		val length=in.readInt
		val readBuffer=new Array[Byte](length)
		in.read(readBuffer,0,length)
		val out=new FileOutputStream(file)
		out.write(readBuffer,0,length)
		out.close	
	}



	def tellToQuit() = {
		System.out.println("tell Quit to "+userName)
		sendData(ServerCommands.wantQuit ) {out =>}
	}
	
	
	def requestUndoData(in:DataInputStream) = {
		System.out.println("User "+userName+" requests Undo data")
		TransactionManager.requestUndoData(userEntry)		
	}
	
	def doUndo(in:DataInputStream) = {
		TransactionManager.doUndo(userEntry)	
	}
	
	def stopUndo(in:DataInputStream) = {
		System.out.println("Stop Undo ")
		TransactionManager.stopUndo(userEntry)
	}
	
	def denyUndoRequest()= {
		sendData(ServerCommands.sendUndoInformation ) {out =>
		out.writeBoolean(false)
		}
	}
	
	def sendUndoLockInfo(undoUserName:String)= {
		sendData(ServerCommands.lockForUndo  ) {out =>
		  out.writeUTF(undoUserName)
		}
	}
	
	def releaseUndoLock() = {
		sendData(ServerCommands.releaseUndoLock  ) {out =>		  
		}
	}
	
	def sendUndoInformation(stepList:Seq[TransStepData]) = {
		//System.out.println(stepList.mkString("\n"))
		sendData(ServerCommands.sendUndoInformation   ) {out =>
		  out.writeInt(stepList.size)
		  System.out.println("StepList size:"+stepList.size)
		  stepList.foreach(_.write(out))
		}
	}
	
	def askEnquiry(question:ParamQuestion,continuation:(UserSocket,Seq[(String,Constant)])=>Unit)= {
		sendData(ServerCommands.askEnquiry ) {out=>
		  out.writeUTF(question.toXML.toString)						
			currentEnquiryContinuation=continuation
		}		
	}
	
	def answerEnquiry(in:DataInputStream) = {
		val numParams=in.readInt
		val paramList=for(i <-0 until numParams) 
					yield (in.readUTF,Expression.readConstant(in))
		if(currentEnquiryContinuation!=null)
			currentEnquiryContinuation(this,paramList)
	}
	
	def sendGeneratedData(func:(DataOutput)=>Unit) = {
		sendData(ServerCommands.sendGeneratedData ) (func)
	}
}

