package operations

class Cipher extends Operation {
  override protected def getNewPosition(secret: Char, msg: Char): Int = position(secret) + position(msg)

  override protected def checkOffset(position: Int): Boolean = position >= end

  override protected def adjustOffset(position: Int): Int = position - end
}
