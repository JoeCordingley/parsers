import cats.data.{State, StateT}
import cats._
import cats.syntax.all._

object Part1:

  type X
  type Y
  type Z

  def parseX(s: String): (String, X) = ???
  def parseY(x: X, s: String): (String, Y) = ???
  def parseZ(y: Y, s: String): (String, Z) = ???

  def parseAllThree(s: String): (String, Z) =
    val (rest, a) = parseX(s)
    val (rest2, b) = parseY(a, rest)
    parseZ(b, rest2)

object Part2:
  import Part1._

  type Parser[A] = (String) => (String, A)

  val xParser: Parser[X] = parseX
  def yParser(x: X): Parser[Y] = parseY(x, _)
  def zParser(y: Y): Parser[Z] = parseZ(y, _)

  def combine[A, B](first: Parser[A], second: A => Parser[B]): Parser[B] = s =>
    val (rest, a) = first(s)
    second(a)(s)

  def parseAllThree(s: String): (String, Z) =
    // combine(combine(xParser, yParser), zParser)(s)
    combine(xParser, y => combine(yParser(y), zParser))(s)

  val xs: List[X] = ???
  def mapParser[A, B](parser: Parser[A], f: A => B): Parser[B] = s =>
    val (rest, a) = parser(s)
    (rest, f(a))

  def parseXs: Parser[List[Y]] =
    xs.foldRight[Parser[List[Y]]](s => (s, List.empty)) { case (x, ysParser) =>
      combine(yParser(x), y => mapParser(ysParser, y :: _))
    }

object Part3:
  import Part1._
  import Part2._

  type Parser[A] = State[String, A]
  // case class State[S, A](run: S => (S, A))

  val xParser: Parser[X] = State(parseX)
  def yParser(x: X): Parser[Y] = State(parseY(x, _))
  def zParser(y: Y): Parser[Z] = State(parseZ(y, _))

  def parseAllThree(s: String): (String, Z) =
    xParser.flatMap(yParser).flatMap(zParser).run(s).value

  def parseXs: Parser[List[Y]] =
    xs.traverse(yParser)

object Part4:
  import Part1._
  import Part2._

  def parseX(s: String): Option[(String, X)] = ???
  def parseY(x: X, s: String): Option[(String, Y)] = ???
  def parseZ(y: Y, s: String): Option[(String, Z)] = ???

  type Parser[A] = StateT[Option, String, A]

  val xParser: Parser[X] = StateT(parseX)
  def yParser(x: X): Parser[Y] = StateT(parseY(x, _))
  def zParser(y: Y): Parser[Z] = StateT(parseZ(y, _))
  def parseAllThree(s: String): Option[(String, Z)] =
    xParser.flatMap(yParser).flatMap(zParser).run(s)

  def parseXs: Parser[List[Y]] =
    xs.traverse(yParser)

object Part5:
  type Parser[A] = StateT[Option, List[Char], A]

  def parse[A](parser: Parser[A], s: String): Option[(String, A)] =
    parser.run(s.toList).map { case (chars, a) =>
      (chars.mkString, a)
    }

  val anyChar: Parser[Char] = StateT {
    case Nil     => None
    case x :: xs => Some((xs, x))
  }

  def ifChar(p: Char => Boolean): Parser[Char] = anyChar.filter(p)
  val digit: Parser[Char] = ifChar(_.isDigit)
  def some[A](fa: Parser[A]): Parser[List[A]] = for {
    a <- fa
    as <- many(fa)
  } yield a :: as

  def many[A](fa: Parser[A]): Parser[List[A]] =
    some(fa) <+> List.empty[A].pure[Parser]

  val nat: Parser[Int] = some(digit).map(_.mkString.toInt)
  def char(c: Char): Parser[Char] = ifChar(_ == c)
  val neg: Parser[Int] = char('-') *> nat.map(-_)
  val int: Parser[Int] = neg <+> nat
  def expr: Parser[Int] = {
    for {
      n <- int
      _ <- char('+')
      e <- expr
    } yield n + e
  } <+> int

object Part6:
  import Part5._
  def expr: Parser[Int] = {
    for {
      n <- term
      _ <- char('+')
      e <- expr
    } yield n + e
  } <+> term

  def term: Parser[Int] = {
    for {
      n <- int
      _ <- char('*')
      e <- term
    } yield n * e
  } <+> int
object Part7:
  import Part6._
  import Part5._
  def expr: Parser[Int] = {
    for {
      n <- term
      _ <- char('+')
      e <- expr
    } yield n + e
  } <+> term

  def term: Parser[Int] = {
    for {
      n <- factor
      _ <- char('*')
      e <- term
    } yield n * e
  } <+> factor

  def factor: Parser[Int] = {
    for {
      _ <- char('(')
      e <- expr
      _ <- char(')')
    } yield e
  } <+> int

object Part8:
  import Part6._
  import Part5._
  def expr: Parser[Int] = {
    for {
      n <- term
      _ <- char('+')
      e <- expr
    } yield n + e
  } <+> {
    for {
      n <- term
      _ <- char('-')
      e <- expr
    } yield n - e
  } <+> term

  // case class StateT
  //
object Part9:
  import Part5._
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
  type Op = (Int, Int) => Int

  def add: Parser[Op] = char('+').as(_ + _)
  def subtract: Parser[Op] = char('-').as(_ - _)
  def multiply: Parser[Op] = char('*').as(_ * _)
  def divide: Parser[Op] = char('/').as(_ / _)

  def expr: Parser[Int] = chainL(term, add <+> subtract)
  def term: Parser[Int] = chainL(factor, multiply <+> divide)

  // same
  def factor: Parser[Int] = {
    for {
      _ <- char('(')
      e <- expr
      _ <- char(')')
    } yield e
  } <+> int

object Part10:
  import Part5._
  import Part9._
//  def main(args: Array[String]): Unit = println(
//    parse(whitespace *> expr, " 5 - 2 /2")
//  )
  val whitespace: Parser[List[Char]] = many(char(' '))
  def lexeme[A](parser: Parser[A]): Parser[A] = parser <* whitespace
  val int: Parser[Int] = lexeme(neg <+> nat)
  def add: Parser[Op] = lexeme(char('+')).as(_ + _)
  def subtract: Parser[Op] = lexeme(char('-')).as(_ - _)
  def multiply: Parser[Op] = lexeme(char('*')).as(_ * _)
  def divide: Parser[Op] = lexeme(char('/')).as(_ / _)
  def factor: Parser[Int] = {
    for {
      _ <- lexeme(char('('))
      e <- expr
      _ <- lexeme(char(')'))
    } yield e
  } <+> int

  // same
  def expr: Parser[Int] = chainL(term, add <+> subtract)
  def term: Parser[Int] = chainL(factor, multiply <+> divide)

object Part11:
  import Part5._

  val pieces: Set[Char] = Set('K', 'Q', 'N', 'B', 'R')
  val files: Set[Char] = ('a' to 'h').toSet
  val ranks: Set[Char] = ('1' to '8').toSet
  def optional[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) <+> Applicative[Parser].pure[Option[A]](None)

  val moveParser: Parser[String] = (
    optional(ifChar(pieces)),
    optional(ifChar(files)),
    optional(ifChar(ranks)),
    optional(char('x')),
    ifChar(files),
    ifChar(ranks)
  ).mapN { (p, from1, from2, x, to1, to2) =>
    (p ++ from1 ++ from2 ++ x ++ List(to1, to2)).mkString
  }
object Part12:

  type Parser[A] = StateT[List, List[Char], A]

//  def main(args: Array[String]): Unit = println(
//    parse(pieceParser, "N")
//  )
  enum Piece:
    case Knight
    case Bishop
    // etc

  val knightParser: Parser[Piece] = char('N').as(Piece.Knight)
  val bishopParser: Parser[Piece] = char('B').as(Piece.Bishop)
  val pieceParser: Parser[Piece] = knightParser <+> bishopParser

  val pieces: Set[Char] = Set('K', 'Q', 'N', 'B', 'R')
  val files: Set[Char] = ('a' to 'h').toSet
  val ranks: Set[Char] = ('1' to '8').toSet
  def optional[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) <+> Applicative[Parser].pure[Option[A]](None)

  def anyChar: Parser[Char] = StateT {
    case Nil     => List.empty
    case x :: xs => List((xs, x))
  }
  def parse[A](parser: Parser[A], s: String): Option[(String, A)] =
    parser.run(s.toList).headOption.map { case (chars, a) =>
      (chars.mkString, a)
    }
  // SAME
  def ifChar(p: Char => Boolean): Parser[Char] = anyChar.filter(p)
  // SAME
  def eof: Parser[Unit] = StateT {
    case Nil => List((Nil, ()))
    case _   => List.empty
  }
  def char(c: Char): Parser[Char] = ifChar(_ == c)

  val moveParser: Parser[String] = (
    optional(ifChar(pieces)),
    optional(ifChar(files)),
    optional(ifChar(ranks)),
    optional(char('x')),
    ifChar(files),
    ifChar(ranks)
  ).mapN { (p, from1, from2, x, to1, to2) =>
    (p ++ from1 ++ from2 ++ x ++ List(to1, to2)).mkString
  }
