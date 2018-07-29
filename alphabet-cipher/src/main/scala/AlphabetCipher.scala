import operations.{Cipher, Decipher, Secret}

object AlphabetCipher {
  private val cipher = new Cipher
  private val decipher = new Decipher
  private val secret = new Secret

  def encode(secret: String, msg: String): String = cipher.transform(secret, msg)

  def decode(secret: String, encoded: String): String = decipher.transform(secret, encoded)

  def secret(encoded: String, msg: String): String = secret.transform(encoded, msg)
}
