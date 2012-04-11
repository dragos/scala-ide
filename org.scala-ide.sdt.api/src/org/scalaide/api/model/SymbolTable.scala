package org.scalaide.api.model

/** Access to the compiler symbol table. */
trait SymbolTable { self: CompilerTypes with Symbols =>

  /** Return the class symbol for the given full name.
   *
   *  @param `fullName` should be an instance of `TypeName`
   *  @return A left-biased instance of Either. Exceptions are returned in `Right`.
   *          If the class is not found, it returns `Right(SymbolNotFound)`.
   */
  def getClass(fullName: TypeName): Either[Symbol, Throwable]

  /** Return the object symbol for the given full name.
   *
   *  @param `fullName` should be an instance of `TermName`
   *  @return A left-biased instance of Either. Exceptions are returned in `Right`.
   *          If the object is not found, it returns `Right(SymbolNotFound)`.
   */
  def getObject(fullName: TermName): Either[Symbol, Throwable]
  
  case class SymbolNotFound(fullName: String) extends RuntimeException("Symbol not found: %s".format(fullName))
}