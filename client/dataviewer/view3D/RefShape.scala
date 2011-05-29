/**
 * Author: Peter Started:27.05.2011
 */
package client.dataviewer.view3D

import definition.data.{Reference,Referencable,OwnerReference}
import javax.media.j3d.Shape3D

/**
 * 
 */
class RefShape(val ref:Reference,val ownerRef:OwnerReference) extends Shape3D with Referencable {

  

}