import cats.data.StateT
import cats.syntax.all._

object JsonParser:
  type Parser[A] = StateT[Option, List[Char], A]

  enum Json:
    case JsonNull
    case JsonBool(value: Boolean)
    case JsonString(value: String)
    case JsonNumber(value: String)
    case JsonArray(values: List[Json])
    case JsonObject(values: Map[String, Json])

  def anyChar: Parser[Char] = StateT {
    case Nil     => None
    case x :: xs => Some((xs, x))
  }
//  def main(args: Array[String]): Unit = println(
//    parse(jsonParser, "true")
//  )
  def parse[A](parser: Parser[A], s: String): Option[(String, A)] =
    parser.run(s.toList).map { case (chars, a) =>
      (chars.mkString, a)
    }

  def ifChar(p: Char => Boolean): Parser[Char] = anyChar.filter(p)
  def char(c: Char): Parser[Char] = ifChar(_ == c)

  def some[A](fa: Parser[A]): Parser[List[A]] = for {
    a <- fa
    as <- many(fa)
  } yield a :: as

  def many[A](fa: Parser[A]): Parser[List[A]] =
    some(fa) <+> List.empty[A].pure[Parser]

  def string(s: String): Parser[String] =
    s.toList.traverse(char).map(_.mkString)

  val nullParser: Parser[Json] = string("null").as(Json.JsonNull)

  val boolParser: Parser[Json] =
    (string("true").as(true) <+> string("false").as(false))
      .map(Json.JsonBool(_))

  val stringParser: Parser[Json] = {
    def specialCharacter(c: Char): Boolean =
      c == '"' || c == '\\' || c.isControl
    val escapedCharacter: Set[Char] =
      Set('"', '\\', '/', 'b', 'f', 'n', 'r', 't')
    val hexValue: Set[Char] =
      ('0' to '9').toSet ++ ('a' to 'e').toSet ++ ('A' to 'E').toSet
    val unicodeSequence: Parser[List[Char]] =
      (char('u') :: List.fill(4)(ifChar(hexValue))).sequence
    val escapeSequence: Parser[List[Char]] = for {
      _ <- char('\\')
      e <- ifChar(escapedCharacter).map(List(_)) <+> unicodeSequence
    } yield '\\' :: e
    for {
      _ <- char('"')
      values <- many(
        ifChar(!specialCharacter(_)).map(List(_)) <+> escapeSequence
      )
      _ <- char('"')
    } yield Json.JsonString(values.flatten.mkString)

  }

  val jsonParser: Parser[Json] = nullParser <+> boolParser
