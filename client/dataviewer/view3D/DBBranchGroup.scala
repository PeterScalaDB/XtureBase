/**
 * Author: Peter Started:24.05.2011
 */
package client.dataviewer.view3D

import javax.media.j3d.{BranchGroup,Group,Node}
import definition.data.{Reference,Referencable}
import collection.JavaConversions._

/**
 * 
 */
class DBBranchGroup(val ref:Reference) extends BranchGroup with Referencable {
    setCapability(BranchGroup.ALLOW_DETACH)
    setCapability(Group.ALLOW_CHILDREN_WRITE)
		setCapability(Group.ALLOW_CHILDREN_EXTEND)
    setCapabilityIsFrequent(0)
    
    
    def updateChild(newValue: DBBranchGroup): Boolean = {
      for(i<-(0 until numChildren);val ch=getChild(i)) ch match {
      	case c:DBBranchGroup => 
      		if(c.ref==newValue.ref) {
      			setChild(newValue,i)
      			return true
      		}
      	case _=>
      }	
      false
    }
    
}