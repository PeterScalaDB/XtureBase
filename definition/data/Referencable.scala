/**
 * Author: Peter Started:31.07.2010
 */
package definition.data

import java.io._

/** superclass of all classes that have the field Ref:Reference
 * These classes can be accessed in a cache
 * 
 */
trait Referencable {
   def ref:Reference=null
   
   def write(d:DataOutput)={}   
   
}