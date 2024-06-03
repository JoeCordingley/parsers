import cats.data.StateT
import cats.syntax.all._
import cats._

object Calculator {
  type Parser[A] = StateT[Option, List[Char], A]
  def parse[A](parser: Parser[A], s: String): Option[A] =
    parser.run(s.toList).flatMap {
      case (Nil, a) => Some(a)
      case _        => None
    }
  val anyChar: Parser[Char] = StateT {
    case Nil     => None
    case x :: xs => Some((xs, x))
  }
  def ifChar(p: Char => Boolean): Parser[Char] = anyChar.filter(p)
  val digit: Parser[Char] = ifChar(_.isDigit)
  val nat: Parser[Int] = some(digit).map(_.mkString.toInt)
  def some[A](fa: Parser[A]): Parser[List[A]] = for {
    a <- fa
    as <- many(fa)
  } yield a :: as

  def many[A](fa: Parser[A]): Parser[List[A]] =
    some(fa) <+> List.empty[A].pure[Parser]

  def char(c: Char): Parser[Char] = ifChar(_ == c)

  val neg: Parser[Int] = char('-') *> nat.map(-_)

  val whitespace: Parser[List[Char]] = many(char(' '))
  def lexeme[A](parser: Parser[A]): Parser[A] = parser <* whitespace
  val int: Parser[Int] = lexeme(neg <+> nat)
  type Op = (Int, Int) => Int
  val add: Parser[Op] = lexeme(char('+')).as(_ + _)
  val subtract: Parser[Op] = lexeme(char('-')).as(_ - _)
  val multiply: Parser[Op] = lexeme(char('*')).as(_ * _)
  val divide: Parser[Op] = lexeme(char('/')).as(_ / _)

  def chainL[A](fa: Parser[A], op: Parser[(A, A) => A]): Parser[A] = {
    def rest(x: A): Parser[A] = {
      for {
        f <- op
        y <- fa
        r <- rest(f(x, y))
      } yield r
    } <+> x.pure[Parser]
    fa.flatMap(rest)
  }
  def expr: Parser[Int] = chainL(term, add <+> subtract)
  def term: Parser[Int] = chainL(factor, multiply <+> divide)

  def factor: Parser[Int] = {
    for {
      _ <- lexeme(char('('))
      e <- expr
      _ <- lexeme(char(')'))
    } yield e
  } <+> int

  def main(args: Array[String]): Unit = println(
    parse(expr, "2 *3 * -6 / -(5 - 2) / 4")
  )

}
