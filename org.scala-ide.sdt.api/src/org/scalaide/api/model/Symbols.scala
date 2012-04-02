package org.scalaide.api.model

import scala.reflect.api.PresentationSymbols

trait Symbols extends PresentationSymbols { self: reflect.api.Names with Types with AnnotationInfos =>

  /** Symbols are completely abstracted away in the Scala model API. All operations
   *  on symbols are performed through `SymbolOps`, whose implementation takes care of
   *  maintaining compiler invariants.
   */
  type Symbol >: Null <: PresentationSymbol

  /** Add operations to symbols. */
  implicit def toSymbolOps(sym: Symbol): SymbolOps

  abstract class SymbolOps(symbol: Symbol) {

    /** A list of annotations attached to this Symbol.
     */
    def annotations: List[self.AnnotationInfo]

    /** Whether this symbol carries an annotation for which the given
     *  symbol is its typeSymbol.
     */
    def hasAnnotation(sym: Symbol): Boolean

    /** Set when symbol has a modifier of the form private[X], NoSymbol otherwise.
     *
     *  Access level encoding: there are three scala flags (PRIVATE, PROTECTED,
     *  and LOCAL) which combine with value privateWithin (the "foo" in private[foo])
     *  to define from where an entity can be accessed.  The meanings are as follows:
     *
     *  PRIVATE     access restricted to class only.
     *  PROTECTED   access restricted to class and subclasses only.
     *  LOCAL       can only be set in conjunction with PRIVATE or PROTECTED.
     *              Further restricts access to the same object instance.
     *
     *  In addition, privateWithin can be used to set a visibility barrier.
     *  When set, everything contained in the named enclosing package or class
     *  has access.  It is incompatible with PRIVATE or LOCAL, but is additive
     *  with PROTECTED (i.e. if either the flags or privateWithin allow access,
     *  then it is allowed.)
     *
     *  The java access levels translate as follows:
     *
     *  java private:     hasFlag(PRIVATE)                && (privateWithin == NoSymbol)
     *  java package:     !hasFlag(PRIVATE | PROTECTED)   && (privateWithin == enclosingPackage)
     *  java protected:   hasFlag(PROTECTED)              && (privateWithin == enclosingPackage)
     *  java public:      !hasFlag(PRIVATE | PROTECTED)   && (privateWithin == NoSymbol)
     */
    def privateWithin: Symbol

    /** For a class: the module or case class factory with the same name in the same package.
     *  For a module: the class with the same name in the same package.
     *  For all others: NoSymbol
     */
    def companionSymbol: Symbol

    /** The type signature of this symbol.
     *  Note if the symbol is a member of a class, one almost always is interested
     *  in `typeSignatureIn` with a site type instead.
     *
     *  @note   Also called `info` in compiler internals.
     *  @return NoType if the presentation compiler failed for some reason.
     */
    def typeSignature: Type // !!! Since one should almost never use this, let's give it a different name.

    /** The type signature of this symbol seen as a member of given type `site`.
     *
     *  @return NoType if the presentation compiler failed for some reason.
     */
    def typeSignatureIn(site: Type): Type

    /** A type reference that refers to this type symbol seen
     *  as a member of given type `site`.
     */
    def asTypeIn(site: Type): Type

    /** The type constructor corresponding to this type symbol.
     *  This is different from `asType` in that type parameters
     *  are part of results of `asType`, but not of `asTypeConstructor`.
     *
     *  Example: Given a class declaration `class C[T] { ... } `, that generates a symbol
     *  `C`. Then `C.asType` is the type `C[T]`, but `C.asTypeConstructor` is `C`.
     */
    def asTypeConstructor: Type // needed by LiftCode

    /** If this symbol is a class, the type `C.this`, otherwise `NoPrefix`.
     */
    def thisPrefix: Type

    /** If this symbol is a class or trait, its self type, otherwise the type
     *  of the symbol itself.
     */
    def selfType: Type

    // tests

    def isBottomClass: Boolean

    /** Is this symbol a type but not a class? */
    def isNonClassType: Boolean

    def isTrait: Boolean

    def isAbstractClass: Boolean
    def isBridge: Boolean
    def isContravariant: Boolean
    def isConcreteClass: Boolean
    def isCovariant: Boolean
    def isEarlyInitialized: Boolean
    def isExistentiallyBound: Boolean
    def isMethod: Boolean
    def isModule: Boolean
    def isModuleClass: Boolean
    def isNumericValueClass: Boolean
    def isOverloaded: Boolean

    def isRefinementClass: Boolean
    
    /** Is the symbol a method, but not a synthetic getter? */
    def isSourceMethod: Boolean
    def isTypeParameter: Boolean
    def isPrimitiveValueClass: Boolean
    def isVarargsMethod: Boolean

    /** Package tests */
    def isEmptyPackage: Boolean
    def isEmptyPackageClass: Boolean
    def isPackage: Boolean
    def isPackageClass: Boolean
    def isRoot: Boolean
    def isRootPackage: Boolean
  }
}