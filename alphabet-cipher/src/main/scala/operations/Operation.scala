package operations

trait Operation {
  private val alphabet = 'a' to 'z'
  private val alphabetMap = (alphabet, init to end).zipped.toMap

  def transform(secret: String, msg: String): String =
    (repeatUntilLength(secret, msg.length), msg)
      .zipped
      .map(operateOnChar)
      .mkString

  private def repeatUntilLength(repeatable: String, length: Int): String =
    if (length <= repeatable.length) repeatable.substring(init, length) else repeatUntilLength(repeatable + repeatable, length)

  private def operateOnChar(secret: Char, msg: Char): Char =
    alphabet.charAt(normalize(getNewPosition(secret, msg)))

  private def normalize(position: Int): Int =
    if (checkOffset(position)) adjustOffset(position) else position

  protected def getNewPosition(secret: Char, msg: Char): Int

  protected def checkOffset(position: Int): Boolean

  protected def adjustOffset(position: Int): Int

  protected def position(position: Char): Int = alphabetMap(position)

  protected def init: Int = 0

  protected def end: Int = alphabet.length
}
