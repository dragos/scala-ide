package org.scalaide.internal.api

import scala.tools.nsc.interactive.Global
import scala.tools.eclipse.contribution.weaving.jdt.IScalaCompilationUnit
import scala.tools.nsc.util.SourceFile

trait SdtCorePresentationCompiler { self: Global =>

  def withSourceFile[T](scu: IScalaCompilationUnit)(op: SourceFile => T): T

  def askReload(scu: IScalaCompilationUnit): Response[Unit]

  def askOption[A](op: => A): Option[A]
}