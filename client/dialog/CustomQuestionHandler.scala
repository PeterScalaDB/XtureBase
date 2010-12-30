/**
 * Author: Peter Started:21.12.2010
 */
package client.dialog

import definition.typ.CustomQuestion
import definition.expression.Constant

/** superclass of all handlers for custom question types in the client
 * 
 */
trait CustomQuestionHandler {
	type AnswerFunc=(Seq[(String,Constant)])=>Unit
	private val answerListeners=collection.mutable.HashSet[AnswerFunc]()
	
	def registerAnswerListener(func:AnswerFunc)= {
		answerListeners+=func
	}
	
	def notifyListeners(answer:Seq[(String,Constant)])= answerListeners.foreach(_(answer))
	
  def load(question:CustomQuestion)
	
}