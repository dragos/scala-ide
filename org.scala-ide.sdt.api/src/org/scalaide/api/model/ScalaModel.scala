package org.scalaide.api.model

import scala.tools.nsc.util.SourceFile
import scala.tools.eclipse.contribution.weaving.jdt.IScalaCompilationUnit

trait ScalaModel { self: CompilerServices =>

  /** Perform `op' on the given compilation unit, seen as a Scala SourceFile. */
  def withSource[A](scu: IScalaCompilationUnit)(op: SourceFile => A): A

  /** Reload the given compilation unit. If this CU is not tracked by the presentation
   *  compiler, it will from now on. Each type-checking pass will consider this source
   *  as well. 
   *  
   *  This method does not wait for the units to be type-checked.
   */
  def askReload(sources: IScalaCompilationUnit*): Unit

  /** Locate the smallest tree that encloses position.
   *
   *  Returns `EmptyTree` if the position could not be found.
   *  @pre Position must be loaded
   */
  def locate(pos: Position): Tree

  /** Locate smallest tree that encloses position.
   *
   *  Returns `EmptyTree` if the position could not be found.
   */
  def locateIn(tree: Tree, pos: Position, p: Tree => Boolean = t => true): Tree

  /** Return the AST of the given compilation unit. The operation may be long-running
   *  if you ask for the fully typed tree.
   *
   *  For any Ast kind other than `Parsed`, `scu` should have been loaded before.
   *
   *  @param  ast The kind of `AST` that is desired. Values are
   *          * `Parsed`: a parse tree is returned. The tree is not attributed (no symbols
   *          nor types have been added to the AST). This operation is fast and can run
   *          concurrently with other compiler jobs.
   *
   *          * `Named`: a tree that has symbols, but that has not yet been typed. This operation
   *          blocks the compiler thread, and it is slower than a plain parser, but still much
   *          faster than a fully-type checked tree. Symbols in the tree may have lazy types, and
   *          inspecting those symbols may trigger additional type-checking.
   *
   *          * `Typed`: a tree that has been fully type-checked. Use cautiously. This operation
   *          is long-running and the presentation compiler won't service other requests until
   *          it's done. Most of the times it's better to request a `Typed` AST using one of the
   *          methods that takes a position argument, since that will type-check the minimum
   *          required to enclose the given position.
   *
   *  @return A left-biased instance of `Either`. Exceptions are returned in the `Right` part.
   *          This is opposite to the conventional use of `Either`.
   *
   *  @note   TODO This might return a `Future` in a future milestone
   */
  def fullTreeOf(scu: IScalaCompilationUnit, ast: AstKind.Value = AstKind.Parsed): Either[Tree, Throwable]

  /** Return the innermost enclosing tree for the given position.
   *
   *
   *  For any Ast kind other than `Parsed`, `scu` should have been loaded before.
   *
   *  @param  ast The kind of `AST` that is desired. Values are
   *          * `Parsed`: a parse tree is returned. The tree is not attributed (no symbols
   *          nor types have been added to the AST). This operation is fast and can run
   *          concurrently with other compiler jobs.
   *
   *          * `Named`: a tree that has symbols, but that has not yet been typed. This operation
   *          blocks the compiler thread, and it is slower than a plain parser, but still much
   *          faster than a fully-type checked tree. Symbols in the tree may have lazy types, and
   *          inspecting those symbols may trigger additional type-checking.
   *
   *          * `Typed`: a tree that has been fully type-checked. Use cautiously. This operation
   *          is long-running and the presentation compiler won't service other requests until
   *          it's done. Most of the times it's better to request a `Typed` AST using one of the
   *          methods that takes a position argument, since that will type-check the minimum
   *          required to enclose the given position.
   *
   *  @return A left-biased instance of Either. Exceptions are returned in the `Right` part.
   *          This is opposite to the conventional use of `Either`.
   *  @note TODO This might return a `Future` in a future milestone
   */
  def treeAt(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[Tree, Throwable]

  /** Return the innermost enclosing class for the given position.
   *
   *  For any Ast kind other than `Parsed`, `scu` should have been loaded before.
   *
   *  @param  ast The kind of `AST` that is desired. Values are
   *          * `Parsed`: a parse tree is returned. The tree is not attributed (no symbols
   *          nor types have been added to the AST). This operation is fast and can run
   *          concurrently with other compiler jobs.
   *
   *          * `Named`: a tree that has symbols, but that has not yet been typed. This operation
   *          blocks the compiler thread, and it is slower than a plain parser, but still much
   *          faster than a fully-type checked tree. Symbols in the tree may have lazy types, and
   *          inspecting those symbols may trigger additional type-checking.
   *
   *          * `Typed`: a tree that has been fully type-checked. Use cautiously. This operation
   *          is long-running and the presentation compiler won't service other requests until
   *          it's done. Most of the times it's better to request a `Typed` AST using one of the
   *          methods that takes a position argument, since that will type-check the minimum
   *          required to enclose the given position.
   *
   *  @return A left-biased instance of Either. Exceptions are returned in the `Right` part.
   *          This is opposite to the conventional use of `Either`.
   *  @note TODO This might return a `Future` in a future milestone
   */
  def enclosingClass(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[Tree, Throwable]

  /** Return the innermost enclosing method definition for the given position.
   *
   *  For any Ast kind other than `Parsed`, `scu` should have been loaded before.
   *
   *  @param  ast The kind of `AST` that is desired. Values are
   *          * `Parsed`: a parse tree is returned. The tree is not attributed (no symbols
   *          nor types have been added to the AST). This operation is fast and can run
   *          concurrently with other compiler jobs.
   *
   *          * `Named`: a tree that has symbols, but that has not yet been typed. This operation
   *          blocks the compiler thread, and it is slower than a plain parser, but still much
   *          faster than a fully-type checked tree. Symbols in the tree may have lazy types, and
   *          inspecting those symbols may trigger additional type-checking.
   *
   *          * `Typed`: a tree that has been fully type-checked. Use cautiously. This operation
   *          is long-running and the presentation compiler won't service other requests until
   *          it's done. Most of the times it's better to request a `Typed` AST using one of the
   *          methods that takes a position argument, since that will type-check the minimum
   *          required to enclose the given position.
   *  @return A left-biased instance of Either. Exceptions are returned in the `Right` part.
   *          This is opposite to the conventional use of `Either`.
   *  @note TODO This might return a `Future` in a future milestone
   */
  def enclosingMethod(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[Tree, Throwable]

  /** Return the top-level enclosing class for the given position.
   * 
   *  For any Ast kind other than `Parsed`, `scu` should have been loaded before.
   *
   *  @param  ast The kind of `AST` that is desired. Values are
   *          * `Parsed`: a parse tree is returned. The tree is not attributed (no symbols
   *          nor types have been added to the AST). This operation is fast and can run
   *          concurrently with other compiler jobs.
   *
   *          * `Named`: a tree that has symbols, but that has not yet been typed. This operation
   *          blocks the compiler thread, and it is slower than a plain parser, but still much
   *          faster than a fully-type checked tree. Symbols in the tree may have lazy types, and
   *          inspecting those symbols may trigger additional type-checking.
   *
   *          * `Typed`: a tree that has been fully type-checked. Use cautiously. This operation
   *          is long-running and the presentation compiler won't service other requests until
   *          it's done. Most of the times it's better to request a `Typed` AST using one of the
   *          methods that takes a position argument, since that will type-check the minimum
   *          required to enclose the given position.
   *  @return A left-biased instance of Either. Exceptions are returned in the `Right` part.
   *          This is opposite to the conventional use of `Either`.
   *  @note TODO This might return a `Future` in a future milestone
   */
  def topLevelClass(pos: Position, ast: AstKind.Value = AstKind.Parsed): Either[Tree, Throwable]

  // Exceptions
  case class PositionNotFound(pos: Position) extends RuntimeException("Position not found: " + pos)
}

/** The kind of AST.
 *
 *  `Parsed` is an AST with no symbol information.
 *  `Named`  is an AST with symbol information, but nothing has been typed yet.
 *           Each symbol holds a lazy type, that can trigger typing in order to
 *           answer methods like `typeSignature` or `members`.
 *  `Typed`  A fully type-checked tree. The AST has full type information. It's
 *           the most expensive type of tree you can ask for.
 */
object AstKind extends Enumeration {
  val Parsed, Named, Typed = Value
}
