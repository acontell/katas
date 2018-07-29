import org.scalatest._

abstract class UnitTest extends FunSuite with Matchers

class AlphabetCipherTest extends UnitTest {

  test("can encode given a secret keyword") {
    AlphabetCipher.encode("vigilance", "meetmeontuesdayeveningatseven") shouldBe "hmkbxebpxpmyllyrxiiqtoltfgzzv"
    AlphabetCipher.encode("scones", "meetmebythetree") shouldBe "egsgqwtahuiljgs"
  }

  test("can decode a crypted message given a secret keyword") {
    AlphabetCipher.decode("vigilance", "hmkbxebpxpmyllyrxiiqtoltfgzzv") shouldBe "meetmeontuesdayeveningatseven"
    AlphabetCipher.decode("scones", "egsgqwtahuiljgs") shouldBe "meetmebythetree"
  }

  test("can extract the secret keyword given an encrypted message and the original message") {
    AlphabetCipher.secret("opkyfipmfmwcvqoklyhxywgeecpvhelzg", "thequickbrownfoxjumpsoveralazydog") shouldBe "vigilance"
    AlphabetCipher.secret("hcqxqqtqljmlzhwiivgbsapaiwcenmyu", "packmyboxwithfivedozenliquorjugs") shouldBe "scones"
    AlphabetCipher.secret("hfnlphoontutufa", "hellofromrussia") shouldBe "abcabcx"
  }
}
