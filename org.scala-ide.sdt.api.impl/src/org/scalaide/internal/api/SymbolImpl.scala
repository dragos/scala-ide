package org.scalaide.internal.api

import org.scalaide.api.model.CompilerServices
import scala.tools.nsc.interactive.Global

trait SymbolImpl extends CompilerServices { self: SdtCorePresentationCompiler with Global =>

  // Symbol is a concrete class in Global, so no need to define an alias
//  type Symbol = self.Symbol

  class RichSymbol(symbol: Symbol) extends SymbolOps(symbol) {

    def annotations: List[SymbolImpl.this.AnnotationInfo] =
      symbol.annotations

    def hasAnnotation(cls: Symbol): Boolean =
      (symbol.annotations.nonEmpty
        && askOption(symbol.hasAnnotation(cls)).getOrElse(false)) // annotations are lazily type checked

    def privateWithin: Symbol =
      symbol.privateWithin

    def companionSymbol: Symbol =
      if (symbol == NoSymbol)
        NoSymbol
      else
        askOption(symbol.companionSymbol).getOrElse(NoSymbol)

    def typeSignature: Type =
      askOption(symbol.typeSignature).getOrElse(NoType)

    def typeSignatureIn(site: Type): Type =
      askOption(symbol.typeSignatureIn(site)).getOrElse(NoType)

    def asTypeIn(site: Type): Type =
      askOption(symbol.asTypeIn(site)).getOrElse(NoType)

    def asTypeConstructor: Type =
      if (symbol.isType)
        askOption(symbol.asTypeConstructor).getOrElse(NoType)
      else
        NoType // fail fast, no need to forward to the presentation compiler in this case 

    def thisPrefix: Type =
      if (symbol.isClass) askOption(symbol.thisPrefix).getOrElse(NoPrefix) else NoPrefix

    def selfType: Type =
      askOption(symbol.selfType).getOrElse(NoType)

    // tests
    def isBottomClass: Boolean = symbol.isBottomClass

    /** Is this symbol a type but not a class? */
    def isNonClassType: Boolean = symbol.isNonClassType

    def isTrait: Boolean = {
      ensureInitialized()
      symbol.isTrait
    }

    def isAbstractClass: Boolean = {
      ensureInitialized()
      symbol.isAbstractClass
    }
    
    def isBridge: Boolean = symbol.isBridge
    
    def isContravariant: Boolean = symbol.isContravariant

    def isCovariant: Boolean = symbol.isCovariant
      
    def isConcreteClass: Boolean = {
      ensureInitialized()
      symbol.isConcreteClass
    }
      
    def isEarlyInitialized: Boolean = symbol.isEarlyInitialized
      
    def isExistentiallyBound: Boolean = symbol.isExistentiallyBound
      
    def isMethod: Boolean = symbol.isMethod
      
    def isModule: Boolean = {
      ensureInitialized()
      symbol.isModule
    }
      
    def isModuleClass: Boolean = {
      ensureInitialized()
      symbol.isModuleClass
    }
    
    def isNumericValueClass: Boolean = symbol.isNumericValueClass
    
    def isOverloaded: Boolean = symbol.isOverloaded

    def isRefinementClass: Boolean = symbol.isRefinementClass

    def isSourceMethod: Boolean = symbol.isSourceMethod

    def isTypeParameter: Boolean = symbol.isTypeParameter
    
    def isPrimitiveValueClass: Boolean = symbol.isPrimitiveValueClass
    
    def isVarargsMethod: Boolean = symbol.isVarargsMethod

    /** Package tests */
    def isEmptyPackage: Boolean       = symbol.isEmptyPackage
    def isEmptyPackageClass: Boolean  = symbol.isEmptyPackageClass
    def isPackage: Boolean            = symbol.isPackage
    def isPackageClass: Boolean       = symbol.isPackageClass
    def isRoot: Boolean               = symbol.isRoot
    def isRootPackage: Boolean        = symbol.isRootPackage

    /** Ensure the symbol has been initialized. If not, call '.initialize' on the
     *  presentation compiler thread. Avoid unnecessarily calling the PC, as that can
     *  saturate the working queue if done too often.
     */
    private def ensureInitialized() {
      if (!symbol.isInitialized)
        askOption(symbol.initialize)
    }
  }
  
  implicit def toSymbolOps(sym: Symbol): SymbolOps = new RichSymbol(sym)
}
