package org.scalaide.api.model


trait Types extends CompilerTypes { self =>

  /** Add operations to a compiler Type. */
  implicit def toTypeOps(tpe: Type): TypeOps
  
  /** This class declares operations that are visible in a Type.
   */
  abstract class TypeOps {
    /** The type symbol associated with the type, or `NoSymbol` for types
     *  that do not refer to a type symbol.
     */
    def typeSymbol: Symbol

    /** The defined or declared members with name `name` in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def declaration(name: Name): Symbol

    /** The collection of declarations in this type
     */
    def declarations: Iterable[Symbol]

    /** The member with given name, either directly declared or inherited,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     */
    def member(name: Name): Symbol

    /** The non-private member with given name, either directly declared or inherited,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     */
    def nonPrivateMember(name: Name): Symbol

    /** An iterable containing all members of this type (directly declared or inherited)
     *  Members appear in the linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     */
    def members: Iterable[Symbol]

    /** An iterable containing all non-private members of this type (directly declared or inherited)
     *  Members appear in the linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     */
    def nonPrivateMembers: Iterable[Symbol]

    /** If this is a parameterized types, the type arguments.
     *  Otherwise the empty list
     */
    def typeArguments: List[Type]

    /** Is this type a type constructor that is missing its type arguments?
     */
    def isHigherKinded: Boolean   // !!! This should be called "isTypeConstructor", no?

    /**
     *  Expands type aliases and converts higher-kinded TypeRefs to PolyTypes.
     *  Functions on types are also implemented as PolyTypes.
     *
     *  Example: (in the below, <List> is the type constructor of List)
     *    TypeRef(pre, <List>, List()) is replaced by
     *    PolyType(X, TypeRef(pre, <List>, List(X)))
     */
    def normalize: Type     // !!! Alternative name? "normalize" is used to mean too many things.

    /** Does this type conform to given type argument `that`? */
    def <:< (that: Type): Boolean

    /** Is this type equivalent to given type argument `that`? */
    def =:= (that: Type): Boolean

    /** The list of all base classes of this type (including its own typeSymbol)
     *  in reverse linearization order, starting with the class itself and ending
     *  in class Any.
     */
    def baseClasses: List[Symbol]   // !!! Alternative name, perhaps linearization?

    /** The erased type corresponding to this type after
     *  all transformations from Scala to Java have been performed.
     */
    def erasedType: Type    // !!! "erasedType", compare with "widen" (so "erase") or "underlying" (so "erased")

    /** Apply `f` to each part of this type, for side effects only */
    def foreach(f: Type => Unit)

    /** Returns optionally first type (in a preorder traversal) which satisfies predicate `p`,
     *  or None if none exists.
     */
    def find(p: Type => Boolean): Option[Type]

    /** Is there part of this type which satisfies predicate `p`? */
    def exists(p: Type => Boolean): Boolean

    /** Does this type contain a reference to given symbol? */
    def contains(sym: Symbol): Boolean

    /** If this is a compound type, the list of its parent types;
     *  otherwise the empty list
     */
    def parents: List[Type]

    /** If this is a singleton type, returns the type underlying it;
     *  otherwise returns this type itself.
     */
    def underlying: Type

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
    def widen: Type
  }

  /** The type of Scala types, and also Scala type signatures.
   *  (No difference is internally made between the two).
   */
  type Type >: Null <: TypeBase

  /** The least upper bound wrt <:< of a list of types */
  def lub(xs: List[Type]): Type

    /** The greatest lower bound wrt <:< of a list of types */
  def glb(ts: List[Type]): Type
}
