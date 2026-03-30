package sttp.tapir.internal

import sttp.model.internal.Rfc3986

import java.nio.charset.Charset
// import scala.scalajs.js.URIUtils

private[tapir] object UrlencodedData {
  def decode(s: String, charset: Charset): Seq[(String, String)] = {
    def decodeSpace(in: String): String = in.replace('+', ' ')
    s.split("&")
      .toList
      .flatMap(kv =>
        kv.split("=", 2) match {
          case Array(k, v) =>
            Some((decodeQuery(decodeSpace(k)), decodeQuery(decodeSpace(v))))
          case _ => None
        }
      )
  }

  def encode(s: Seq[(String, String)], charset: Charset): String = {
    def encodeSpace(in: String): String = in.replace("%20", "+")

    s.map { case (k, v) =>
      s"${encodeSpace(encodeQuery(k))}=${encodeSpace(encodeQuery(v))}"
    }.mkString("&")
  }

  def encode(s: String): String = {
    encodeQuery(s)
  }

  def encodePathSegment(s: String): String = {
    Rfc3986.encode(Rfc3986.PathSegment)(s)
  }

  private def decodeQuery(s: String): String = {
    val result = new StringBuilder()
    var i = 0
    while (i < s.length) {
      val char = s.charAt(i)
      if (char == '%' && i + 2 < s.length) {
        val hex = s.substring(i + 1, i + 3)
        try {
          val byte = Integer.parseInt(hex, 16)
          result.append(byte.toChar)
          i += 3
        } catch {
          case _: NumberFormatException =>
            result.append(char)
            i += 1
        }
      } else {
        result.append(char)
        i += 1
      }
    }
    result.toString()
  }

  private def encodeQuery(s: String): String = {
    val result = new StringBuilder()
    s.foreach { char =>
      if (isUnreserved(char)) {
        result.append(char)
      } else if (char.toInt < 128) {
        // ASCII character - percent encode it
        val byte = char.toInt
        result.append('%')
        result.append(toHex((byte >> 4) & 0xF))
        result.append(toHex(byte & 0xF))
      }
      // Skip non-ASCII characters
      // WASM-TODO
    }
    result.toString()
  }

  private def isUnreserved(char: Char): Boolean = {
    (char >= 'A' && char <= 'Z') ||
    (char >= 'a' && char <= 'z') ||
    (char >= '0' && char <= '9') ||
    char == '-' || char == '_' || char == '.' || char == '~'
  }

  private def toHex(value: Int): Char = {
    if (value < 10) ('0'.toInt + value).toChar
    else ('A'.toInt + (value - 10)).toChar
  }

}
