/**
 * Author: Peter Started:27.10.2010
 */
package client.graphicsView


import scala.swing.{BoxPanel,Component,Button}


/** "new panel" for the graphics view
 * 
 */
class GraphicsNewPanel extends BoxPanel(scala.swing.Orientation.Vertical){
  val lineBut=new Button("Linie")
  val kreisBut=new Button("Kreis")
  val parBut=new Button("Parallele")
  
  contents+=lineBut+=kreisBut+=parBut

}