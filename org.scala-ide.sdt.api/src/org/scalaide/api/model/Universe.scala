package org.scalaide.api.model

/**
 * The equivalent of Universe in the Scala compiler.
 */
trait Universe extends CompilerTypes
  with Symbols
  with Types
  with reflect.api.TreePrinters
  with reflect.api.Trees
  with AnnotationInfos
