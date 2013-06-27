package scala.tools.eclipse.debug

import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant
import scala.tools.eclipse.debug.model.ScalaStackFrame
import scala.tools.eclipse.debug.async.AsyncStackFrame

/**
 * SourceLookupParticipant providing a source names when using the
 * Scala debugger
 */
object ScalaSourceLookupParticipant extends AbstractSourceLookupParticipant {

  def getSourceName(obj: AnyRef): String = {
    obj match {
      case stackFrame: ScalaStackFrame =>
        stackFrame.getSourcePath
      case sf: AsyncStackFrame =>
        sf.getSourcePath
      case _ =>
        null
    }
  }

}