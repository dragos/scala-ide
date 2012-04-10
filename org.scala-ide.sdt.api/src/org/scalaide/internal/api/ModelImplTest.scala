package org.scalaide.internal.api

import scala.tools.eclipse.ScalaPresentationCompiler
import org.scalaide.api.model._

object ModelImplTest extends ScalaPresentationCompiler(null, null)
  with Universe
  with SymbolImpl
  with TypeImpl
  with ScalaModelImpl
  with AnnotationInfoImpl