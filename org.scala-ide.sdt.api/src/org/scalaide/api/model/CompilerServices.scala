package org.scalaide.api.model

/**
 * The equivalent of Universe in the Scala compiler.
 * 
 * All compiler services can be accessed through this type. It contains
 * 
 */
trait CompilerServices extends CompilerTypes
  with Symbols
  with Types
//  with AnnotationInfos
  with SymbolTable
