package org.scalaide.api.model

import scala.tools.nsc.interactive.Compatibility

/**
 * The equivalent of Universe in the Scala compiler.
 */
trait Universe extends CompilerTypes
  with Symbols
  with Types
  with AnnotationInfos
