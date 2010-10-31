/**
 * Author: Peter Started:29.08.2010
 */
package server.config

/**
 * 
 */
case class UserInfo(name:String,id:Short,password:String ) {
	
	def toXML():scala.xml.Node = {
		<User name={name} id={id.toString} password={password}> </User>
	}

}




object UserInfo {
	def fromXML(node: scala.xml.Node)= {
		val name=(node \"@name").text
		val id=(node \"@id").text.toShort
		val password = (node \"@password").text
		UserInfo(name,id,password)
	}
}


object UserList {
	var list:Map[String,UserInfo]=Map()
	
	def toXML():scala.xml.Node = {
		val l=list.map(x => x._2.toXML)
		
		<UserList> {for (u <-list) yield u._2 .toXML} </UserList>
	}
	
	def fromXML(node: scala.xml.Node)= {
		list =  (for(userNode <-(node \\"User");newUser=UserInfo.fromXML(userNode))
			 yield {(newUser.name -> newUser) }).toMap
	}
	
	def addUser(newUser:UserInfo) = {
		list = list + (newUser.name -> newUser)
	}
}