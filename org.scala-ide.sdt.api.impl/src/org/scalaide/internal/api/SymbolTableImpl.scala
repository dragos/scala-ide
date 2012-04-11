package org.scalaide.internal.api

import org.scalaide.api.model.SymbolTable
import org.scalaide.api.model.CompilerServices
import scala.tools.nsc.interactive.Global

/** Implementation class for [[SymbolTable]] operations.  */
trait SymbolTableImpl extends SymbolTable { 
  self: CompilerServices 
   with SdtCorePresentationCompiler
   with Global =>
     
  /** Return the class symbol for the given full name.
   *
   *  @param `fullName` should be an instance of `TypeName`
   *  @return A left-biased instance of Either. Exceptions are returned in `Right`.
   *          If the class is not found, it returns `Right(SymbolNotFound)`.
   */
  def getClass(fullName: TypeName): Either[Symbol, Throwable] =
    askOption(definitions.getClass(fullName)) match {
      case Some(sym)             => Left(sym)
      case None | Some(NoSymbol) => Right(SymbolNotFound("class " + fullName.toString))
    }

  /** Return the object symbol for the given full name.
   *
   *  @param `fullName` should be an instance of `TermName`
   *  @return A left-biased instance of Either. Exceptions are returned in `Right`.
   *          If the object is not found, it returns `Right(SymbolNotFound)`.
   */
  def getObject(fullName: TermName): Either[Symbol, Throwable] =
    askOption(definitions.getModule(fullName)) match {
      case Some(sym)             => Left(sym)
      case None | Some(NoSymbol) => Right(SymbolNotFound("object " + fullName.toString))
    }

}