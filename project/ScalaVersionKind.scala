import sbt.CrossVersion

sealed trait ScalaVersionKind

object ScalaVersionKind {
  case object Scala2 extends ScalaVersionKind
  case object Scala3 extends ScalaVersionKind

  def from(scalaVersion: String): ScalaVersionKind =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, _)) => Scala2
      case Some((3, _)) => Scala3
      case _            => sys.error(s"Unsupported Scala version: $scalaVersion")
    }
}
