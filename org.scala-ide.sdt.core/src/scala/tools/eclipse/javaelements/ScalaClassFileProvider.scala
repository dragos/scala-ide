/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.eclipse
package javaelements

import java.io.ByteArrayInputStream

import scala.collection.immutable
import scala.tools.eclipse.ScalaClassFileDescriber
import scala.tools.eclipse.ScalaPlugin
import scala.tools.eclipse.contribution.weaving.jdt.cfprovider.IClassFileProvider
import scala.tools.eclipse.logging.HasLogger

import org.eclipse.jdt.core.IClassFile
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IPackageFragmentRoot
import org.eclipse.jdt.internal.core.ClassFile
import org.eclipse.jdt.internal.core.PackageFragment

class ScalaClassFileProvider extends IClassFileProvider with HasLogger {

  override def create(contents: Array[Byte], parent: PackageFragment, name: String): ClassFile = {
    def updateCache(isScalaClassfile: Boolean) {
      val pfr = parent.getPackageFragmentRoot()
      if ((pfr ne null) && !scalaPackageFragments.isDefinedAt(pfr)) {
        logger.debug(s"Setting ${pfr.getElementName} (becasue of class $name) to be ${if (isScalaClassfile) "Scala" else "Java"}")
        synchronized { scalaPackageFragments += pfr -> isScalaClassfile }
      }
    }

    val scalaCF = ScalaClassFileDescriber.isScala(new ByteArrayInputStream(contents)) match {
      case Some(sourcePath) => new ScalaClassFile(parent, name, sourcePath)
      case _                => null
    }
    updateCache(scalaCF ne null)
    scalaCF
  }

  /** Return `true` if the classfile could be a Scala classfile.
   *
   *  @note This method caches the result of the first classfile read from a package fragment (usually a jar).
   *        This heuristic might fail if a single jar mixes Java and Scala classfiles.
   */
  override def isInteresting(classFile: IClassFile): Boolean = {
    if (ScalaPlugin.plugin.isScalaProject(classFile.getJavaProject)) {
      val pfr = ancestorFragmentRoot(classFile)
      val stillInteresting = pfr.map(scalaPackageFragments.getOrElse(_, true)).getOrElse(true)
      if (!stillInteresting)
        logger.debug(s"Cached result for ${classFile.getElementName}: ${pfr.map(_.getElementName())}. is a Java jar")

      stillInteresting
    } else
      false
  }

  private def ancestorFragmentRoot(classFile: IClassFile): Option[IPackageFragmentRoot] =
    classFile.getAncestor(IJavaElement.PACKAGE_FRAGMENT_ROOT) match {
      case pfr: IPackageFragmentRoot => Some(pfr)
      case _                         => None
    }

  @volatile
  private var scalaPackageFragments: Map[IPackageFragmentRoot, Boolean] = new immutable.HashMap()
}
