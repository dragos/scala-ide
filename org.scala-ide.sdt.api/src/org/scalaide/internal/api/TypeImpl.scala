package org.scalaide.internal.api

import scala.tools.eclipse.ScalaPresentationCompiler
import org.scalaide.api.model.Types
import org.scalaide.api.model.Universe

/** The type implementation. Types are fully abstract in the
 *  public interface, and methods are added through an implicit
 *  conversion. The conversion takes care of wrapping non-thread
 *  safe operations in calls to 'ask'.
 */
trait TypeImpl extends Universe { self: ScalaPresentationCompiler =>

  type Type = self.Type

  class RichType(tpe: Type) extends TypeOps {

    /** The type symbol associated with the type, or `NoSymbol` for types
     *  that do not refer to a type symbol.
     */
    def typeSymbol: Symbol =
      if (tpe.isComplete) tpe.typeSymbol
      else askOption(tpe.typeSymbol).getOrElse(NoSymbol)

    /** The defined or declared members with name `name` in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def declaration(name: Name): Symbol =
      askOption(tpe.declaration(name)).getOrElse(NoSymbol) // because the compiler may need to create a fresh new OverloadedSymbol

    /** The collection of declarations in this type
     */
    def declarations: Iterable[Symbol] =
      askOption(tpe.declarations).getOrElse(EmptyScope)

    /** The member with given name, either directly declared or inherited,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     */
    def member(name: Name): Symbol =
      askOption(tpe.member(name)).getOrElse(NoSymbol)

    /** The non-private member with given name, either directly declared or inherited,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     */
    def nonPrivateMember(name: Name): Symbol =
      askOption(tpe.nonPrivateMember(name)).getOrElse(NoSymbol)

    /** An iterable containing all members of this type (directly declared or inherited)
     *  Members appear in the linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     */
    def members: Iterable[Symbol] = 
      askOption(tpe.members).getOrElse(EmptyScope)

    /** An iterable containing all non-private members of this type (directly declared or inherited)
     *  Members appear in the linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     */
    def nonPrivateMembers: Iterable[Symbol] =
      askOption(tpe.nonPrivateMembers).getOrElse(EmptyScope)

    /** If this is a parameterized types, the type arguments.
     *  Otherwise the empty list
     */
    def typeArguments: List[Type] = 
      askOption(tpe.typeArguments).getOrElse(Nil)

    /** Is this type a type constructor that is missing its type arguments?
     */
    def isHigherKinded: Boolean =
      askOption(tpe.isHigherKinded).getOrElse(false)

    /** Expands type aliases and converts higher-kinded TypeRefs to PolyTypes.
     *  Functions on types are also implemented as PolyTypes.
     *
     *  Example: (in the below, <List> is the type constructor of List)
     *    TypeRef(pre, <List>, List()) is replaced by
     *    PolyType(X, TypeRef(pre, <List>, List(X)))
     */
    def normalize: Type =
      askOption(tpe.normalize).getOrElse(NoType)

    /** Does this type conform to given type argument `that`? */
    def <:<(that: Type): Boolean =
      askOption(tpe <:< that).getOrElse(false)

    /** Is this type equivalent to given type argument `that`? */
    def =:=(that: Type): Boolean = 
      askOption(tpe =:= that).getOrElse(false)

    /** The list of all base classes of this type (including its own typeSymbol)
     *  in reverse linearization order, starting with the class itself and ending
     *  in class Any.
     *  
     *  TODO: Can this be called without an askOption?
     */
    def baseClasses: List[Symbol] =
      askOption(tpe.baseClasses).getOrElse(Nil)

    /** The erased type corresponding to this type after
     *  all transformations from Scala to Java have been performed.
     */
    def erasedType: Type =
      askOption(tpe.erasedType).getOrElse(NoType)

    /** Apply `f` to each part of this type, for side effects only */
    def foreach(f: Type => Unit) = 
      if (tpe.isComplete) tpe.foreach(f)
      else askOption(tpe.foreach(f)).getOrElse(())

    /** Returns optionally first type (in a preorder traversal) which satisfies predicate `p`,
     *  or None if none exists.
     */
    def find(p: Type => Boolean): Option[Type] =
      if (tpe.isComplete) tpe.find(p)
      else askOption(tpe.find(p)).getOrElse(None)

    /** Is there part of this type which satisfies predicate `p`? */
    def exists(p: Type => Boolean): Boolean = 
      if (tpe.isComplete) tpe.exists(p)
      else askOption(tpe.exists(p)).getOrElse(false)

    /** Does this type contain a reference to given symbol? */
    def contains(sym: Symbol): Boolean =
      if (tpe.isComplete) tpe.contains(sym)
      else askOption(tpe.contains(sym)).getOrElse(false)

    /** If this is a compound type, the list of its parent types;
     *  otherwise the empty list
     */
    def parents: List[Type] =
      if (tpe.isComplete) tpe.parents
      else askOption(tpe.parents).getOrElse(Nil)

    /** If this is a singleton type, returns the type underlying it;
     *  otherwise returns this type itself.
     */
    def underlying: Type =
      if (tpe.isComplete) tpe.underlying
      else askOption(tpe.underlying).getOrElse(NoType)

    /** If this is a singleton type, widen it to its nearest underlying non-singleton
     *  base type by applying one or more `underlying` dereferences.
     *  If this is not a singleton type, returns this type itself.
     *
     *  Example:
     *
     *  class Outer { class C ; val x: C }
     *  val o: Outer
     *  <o.x.type>.widen = o.C
     */
    def widen: Type =
      if (tpe.isComplete) tpe.widen
      else askOption(tpe.widen).getOrElse(NoType)
  }
  
  implicit def toTypeOps(tpe: Type): TypeOps = new RichType(tpe)  
}
