package org.scalaide.internal.api

import scala.tools.eclipse.ScalaPresentationCompiler
import org.scalaide.api.model.Universe

trait AnnotationInfoImpl extends Universe { self: ScalaPresentationCompiler =>

  type AnnotationInfo = self.AnnotationInfo

  class RichAnnotation(ann: AnnotationInfo) extends AnnotationOps {

    /** The type of this annotation. */
    def annotationType: Type = ann match {
      case lazyAnn: LazyAnnotationInfo =>
        askOption(ann.annotationType).getOrElse(NoType)
      case _ =>
        ann.annotationType
    }

    /** The arguments of this annotation. */
    def args: List[Tree] = ann match {
      case lazyAnn: LazyAnnotationInfo =>
        askOption(lazyAnn.args).getOrElse(Nil)
      case _ => 
        ann.args
    }

    /** The position of this annotation. */
    def pos: Position = ann.pos

    /** The symbol corresponding to this annotation. */
    def symbol: Symbol = {
      // we always wrap the call in an ask because there are two levels of
      // laziness that could hit us: the annotation could be lazy, and it's 
      // type could be lazy. too mouch trouble to optimize this call for
      // the case when both are completed, so we always put it in an ask.
      askOption(ann.symbol).getOrElse(NoSymbol)
    }

    /** The original tree from which this annotation info was computed. */
    def original: Tree = ann match {
      case lazyAnn: LazyAnnotationInfo =>
        askOption(lazyAnn.original).getOrElse(EmptyTree)
      case _ => 
        ann.original
    }
  }
  
  implicit def annotationOps(ann: AnnotationInfo): AnnotationOps = new RichAnnotation(ann)
}