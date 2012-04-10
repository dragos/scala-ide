package org.scalaide.api.model

trait AnnotationInfos { self: Universe =>

  type AnnotationInfo <: AnyRef

  abstract class AnnotationOps {
    
    /** The type of this annotation. */
    def annotationType: Type
    
    /** The arguments of this annotation. */
    def args: List[Tree]
    
    /** The position of this annotation. */
    def pos: Position
    
    /** The symbol corresponding to this annotation. */
    def symbol: Symbol
    
    /** The original tree from which this annotation info was computed. */
    def original: Tree
  }
  
  implicit def annotationOps(ann: AnnotationInfo): AnnotationOps
}
