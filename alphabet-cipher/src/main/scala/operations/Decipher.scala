package operations

class Decipher extends Operation {
  override protected def getNewPosition(secret: Char, encoded: Char): Int = position(encoded) - position(secret)

  override protected def checkOffset(position: Int): Boolean = position < init

  override protected def adjustOffset(position: Int): Int = position + end
}
