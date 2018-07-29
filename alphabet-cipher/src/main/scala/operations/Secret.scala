package operations

import java.util.regex.{Matcher, Pattern}

class Secret extends Decipher {
  private val errorMsg = "ERROR"
  // https://stackoverflow.com/questions/44465284/match-longest-repeating-sequence-that-is-not-made-of-a-repeating-sequence
  private val secretPattern = Pattern.compile("(?=(.+)\\1+(.*))(?=(.+?)\\3+\\2$)\\3+")
  private val secretGroup = 3

  override protected def getNewPosition(encoded: Char, msg: Char): Int = position(encoded) - position(msg)

  override def transform(encoded: String, msg: String): String =
    getSecretOrError(secretPattern.matcher(super.transform(encoded, msg)))

  private def getSecretOrError(matcher: Matcher): String = if (matcher.find()) matcher.group(secretGroup) else errorMsg
}
