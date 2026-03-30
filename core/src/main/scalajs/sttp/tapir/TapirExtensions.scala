package sttp.tapir

// import org.scalajs.dom.File

trait TapirExtensions {
  type TapirFile = Any
}

object TapirFile {
  def name(f: TapirFile): String = throw new Exception("TapirFile.name")
}
