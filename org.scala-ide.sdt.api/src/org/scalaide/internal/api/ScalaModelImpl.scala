package org.scalaide.internal.api

import org.scalaide.api.model._
import scala.tools.eclipse.ScalaPresentationCompiler
import scala.tools.eclipse.javaelements.ScalaCompilationUnit
import scala.tools.nsc.util.SourceFile

trait ScalaModelImpl extends ScalaModel { self: Universe with ScalaPresentationCompiler =>

  private val TIMEOUT = 5000

  private def ??? = sys.error("not implemented yet")

  def withSource[A](scu: ScalaCompilationUnit)(op: SourceFile => A): A =
    self.withSourceFile(scu)((srcFile, compiler) => op(srcFile))

  /** Reload the given compilation unit. If this CU is not tracked by the presentation
   *  compiler, it will from now on. Each type-checking pass will consider this source
   *  as well.
   */
  def askReload(sources: ScalaCompilationUnit*): Unit =
    sources foreach { scu => self.askReload(scu, scu.getContents) }

  /** Locate smallest tree that encloses position
   *  @pre Position must be loaded
   */
  def locateTree(pos: Position): Tree = self.locateTree(pos)

  /** Locate smallest tree that encloses position.
   *
   *  Returns `EmptyTree` if the position could not be found.
   */
  def locateIn(tree: Tree, pos: Position, p: Tree => Boolean = t => true): Tree =
    new FilteringLocator(pos, p) locateIn tree

  class FilteringLocator(pos: Position, p: Tree => Boolean) extends Locator(pos) {
    override def isEligible(t: Tree) = super.isEligible(t) && p(t)
  }

  /** Return the AST of the given compilation unit. The operation may be long-running
   *  if you ask for the fully typed tree.
   */
  def fullTreeOf(scu: ScalaCompilationUnit, ast: AstKind.Value = AstKind.Parsed): Either[Tree, Throwable] = {
    val response = new Response[Tree]

    ast match {
      case AstKind.Parsed => Left(withSource(scu)(self.parseTree))
      case AstKind.Typed =>
        withSource(scu) { self.askLoadedTyped(_, response) }
        response.get
      case AstKind.Named =>
        withSource(scu) { src => self.askStructure(false)(src, response) }
        response.get
    }
  }

  /** Return the innermost enclosing tree for the given position.
   *
   *  @return A left-biased instance of Either. Exceptions are returned in the `Right` part.
   */
  def treeAt(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[Tree, Throwable] = {
    val response = new Response[Tree]

    ast match {
      case AstKind.Parsed =>
        val tree = self.parseTree(pos.source)
        Left(locateIn(tree, pos))

      case AstKind.Named | AstKind.Typed =>
        self.askTypeAt(pos, response)
        response.get
    }
  }

  /** Return the innermost enclosing class for the given position.
   *
   *  @return A left-biased instance of Either. Exceptions are returned in the `Right` part.
   */
  def enclosingClass(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[ClassDef, Throwable] = {
    val eitherCdef = locateIn(parseTree(pos.source), pos, _.isInstanceOf[ClassDef]) match {
      case c: ClassDef => Left(c)
      case EmptyTree   => Right(PositionNotFound(pos))
    }

    ast match {
      case AstKind.Parsed => eitherCdef
      case AstKind.Named | AstKind.Typed =>
        val response = new Response[Tree]
        eitherCdef.left.map(cdef => self.askTypeAt(cdef.pos, response))
        response.get.left.map(_.asInstanceOf[ClassDef])
    }
  }

  /** Return the innermost enclosing method definition for the given position.
   *
   *  @return A right-biased instance of Either. Exceptions are returned in the 'left' part.
   */
  def enclosingMethod(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[DefDef, Throwable] = ???

  /** Return the top-level enclosing class for the given position.
   *
   *  @return A right-biased instance of Either. Exceptions are returned in the 'left' part.
   */
  def topLevelClass(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[ClassDef, Throwable] = ???
}
