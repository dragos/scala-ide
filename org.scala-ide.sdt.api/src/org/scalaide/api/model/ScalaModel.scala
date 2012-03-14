package org.scalaide.api.model

trait ScalaModel { self: Universe =>

  /** Return the innermost enclosing tree for the given position.
   * 
   *  @return A right-biased instance of either. Exceptions are return in the 'left' part.
   */
  def treeOf(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[Throwable, Tree]
  
  /** Return the innermost enclosing class for the given position.
   * 
   *  @return A right-biased instance of either. Exceptions are return in the 'left' part.
   */
  def enclosingClass(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[Throwable, ClassDef]
  
  /** Return the innermost enclosing method definition for the given position.
   * 
   *  @return A right-biased instance of either. Exceptions are return in the 'left' part.
   */
  def enclosingMethod(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[Throwable, DefDef]
  
  /** Return the top-level enclosing class for the given position.
   * 
   *  @return A right-biased instance of either. Exceptions are return in the 'left' part.
   */
  def topLevelClass(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[Throwable, ClassDef]
}


object AstKind extends Enumeration {
  val Parsed, Named, Typed = Value
}