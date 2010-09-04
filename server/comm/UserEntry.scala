/**
 * Author: Peter Started:04.09.2010
 */
package server.comm

import server.config._

/** a member of the ActiveUsers list
 * 
 */
case class UserEntry(info:UserInfo,thread:UserSocket,queryHandler:UserQueryHandler)