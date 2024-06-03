import cats.data.{State, StateT}
import cats._
import cats.syntax.all._

object Talk:
  type X
  type Y
  type Z

  def parseX(s: String): (String, X) = ???
  def parseY(x: X, s: String): (String, Y) = ???
  def parseZ(x: Y, s: String): (String, Z) = ???

  type Parser[A] = State[String, A]

  val xParser: Parser[X] = State(parseX)
  def yParser(x: X): Parser[Y] = State(parseY(x, _))
  def zParser(x: Y): Parser[Z] = State(parseZ(x, _))

  val xs: List[X] = ???

  val ys: Parser[List[Y]] = xs.traverse(yParser)

  def parseAllThree(s: String): (String, Z) =
    xParser.flatMap(yParser).flatMap(zParser).run(s).value
