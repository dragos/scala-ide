package org.scalaide.api.model

/** Term and type names in Scala programs.
 * 
 *  Scala has two namespaces: one for types and one for terms, meaning that 
 *  you can have a type and a term with the same name. The most obvious example
 *  is the case of companion classes and objects.
 *  
 *  The Scala compiler uses its own representation for names which allows this differentiation.
 *  A Term name is never equal to a Type name, even when their string representation is the same.
 *  
 *  Names are fully opaque types in the API. Only methods inherited from `AnyRef` can be called.
 *  For any string-related operations convert the name to a `String` first.
 */
trait Names {

  type Name <: AnyRef
  type TermName <: Name
  type TypeName <: Name

  /** Create a new term name.   */
  def newTermName(s: String): TermName

  /** Creates a new type name.   */
  def newTypeName(s: String): TypeName

  def EmptyTermName: TermName = newTermName("")
  def EmptyTypeName: TypeName = newTypeName("")

  trait NameOps {
    val self: Name
    
    /** Is the given name a term name? */
    def isTermName: Boolean

    /** Is the given name a type name? */
    def isTypeName: Boolean
  }

  implicit def toNameOps(nme: Name): NameOps
}
