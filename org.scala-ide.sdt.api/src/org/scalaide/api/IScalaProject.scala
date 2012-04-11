package org.scalaide.api

import org.scalaide.api.model.CompilerServices
import org.eclipse.core.runtime.IPath
import org.eclipse.jface.preference.IPreferenceStore
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.core.resources.IProject

/** A Scala project.
 */
trait IScalaProject {

  /** Perform `op` in the current Scala compiler universe.
   *
   *  [[CompilerServices]] provides both types and methods to manipulate compiler
   *  data structures. The presentation compiler running in the background
   *  can restart at any time (for instance because of a clean build or a classpath
   *  refresh), therefore __no values retrieved through `CompilerServices`
   *  should be stored outside the scope of the services oject__.
   */
  def withCompilerServices[T](op: CompilerServices => T): T

  /** Return all source folders of this project. */
  def sourceFolders: Seq[IPath]

  /** Return all output folders of this project. */
  def outputFolders: Seq[IPath]

  /** Return the resolved classpath of this project.
   *
   *  Classpath containers or dependent projects are resolved to their respective
   *  jars or output folders.
   */
  def classpath: Seq[IPath]

  /** Return the preference store used by this project. If the project uses
   *  project-specific settings it returns the corresponding store, otherwise
   *  it returns the plugin-wide preference store.
   */
  def preferenceStore: IPreferenceStore

  /** The underlying platform project. */
  def underlyingProject: IProject

  /** The underlying Java project. */
  def underlyingJavaProject: IJavaProject
}
