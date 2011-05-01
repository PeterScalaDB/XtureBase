/**
 * Author: Peter Started:25.04.2011
 */
package client.print

import javax.swing.AbstractListModel
import definition.data.Reference
import client.comm.ClientQueryManager

/**
 * 
 */
class ArchiveListModel extends AbstractListModel {
  
	var archiveList:Seq[ArchivePageable]= Seq.empty
	
	
	def load(outDefRef:Reference) = {
		archiveList=ClientQueryManager.queryInstance(outDefRef,1).map(new ArchivePageable(_))
		fireContentsChanged(this,0,archiveList.size-1)
	}
	
	def clear={
		archiveList=Seq.empty
		fireIntervalRemoved(this,0,0)
	}
	
	
	
	
	
	
  def getSize(): Int = { archiveList.size }

  def getElementAt(index: Int): Object = { archiveList(index) }

}