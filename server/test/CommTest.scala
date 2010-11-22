/**
 * Author: Peter Started:29.08.2010
 */
package server.test

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import definition.typ._
import definition.expression._
import org.junit._
import server.config._
import server.storage._
import definition.data._
import transaction.handling._



/**
 * 
 */
class CommTest extends JUnitSuite {
	
	@Test def createUserTest() = {
		UserList.addUser(new UserInfo("Peter",10,"pass"))
		UserList.addUser(new UserInfo("Skywalker",12,"force"))
		UserList.addUser(new UserInfo("Moonwalker",13,"jacko"))
		val file=FSPaths.configDir+"users.xml"
  	scala.xml.XML.save(file,UserList.toXML,"UTF-8",true,null)
		UserList.fromXML(xml.XML.loadFile(file))
		println(UserList.toXML)
	}

}