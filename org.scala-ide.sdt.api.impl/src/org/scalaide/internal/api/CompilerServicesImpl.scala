package org.scalaide.internal.api

import org.scalaide.api.model.CompilerServices
import scala.tools.nsc.interactive.Global

trait CompilerServicesImpl extends CompilerServices
  with SdtCorePresentationCompiler
  with SymbolImpl
  with TypeImpl
  with ScalaModelImpl
  with AnnotationInfoImpl
  with SymbolTableImpl { self: Global => }
