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
   
   
   /** writes the Referencable object in the given datastream
    *  subclasses dont need to store the reference in the datastream
    *  
    * @param d
    */
   def write(d:DataOutput)={}   
   
}