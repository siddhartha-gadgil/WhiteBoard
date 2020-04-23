package whiteboard

import fastparse._, NoWhitespace._

sealed trait Content

sealed trait Span extends Content

sealed trait Div extends Content

object Content {

  case class Text(body: String) extends Span

  case class Strong(body: String) extends Span

  case class Emph(body: String) extends Span


  case class Heading(spans: List[Span], level: Int) extends Div

  case class InlineTeX(code: String) extends Span

  case class DisplayTeX(code: String) extends Div

  case class Paragraph(spans: List[Span]) extends Div

  val eg = Paragraph(
    List(Text("Something like"), InlineTeX("x^2 + y^2"), Text(" is a formula"))
  )

  def inline[_: P]: P[Span] =
    P(" ".rep ~ "$" ~ (CharPred(x => x != '$').rep(1)).! ~ "$").map(s =>
      InlineTeX(s)
    )

  def blankLine[_: P] : P[Unit] = P(" ".rep ~ "\n").rep(2)

  def letter[_: P]: P[String] = !blankLine ~ CharPred(x => !Set('$', '_').contains(x)).! //.map(s => Text(s.toString()))

//   def prepend(x: Span, ys: List[Span]): List[Span] = (x, ys) match {
//     case (Text(a), Text(w) :: tail) => Text(a + w) :: tail
//     case (a, bs)                    => a :: bs
//   }

  def word[_: P] : P[Span] = letter.rep(1).map(l => Text(l.mkString("")))

  def strong[_: P] : P[Span] = P("__" ~letter.rep(1) ~ "__").map(l => Strong(l.mkString("")))

  def emph[_: P] : P[Span] = P("_" ~letter.rep(1) ~ "_").map(l => Emph(l.mkString("")))


  def span[_: P]: P[Span] = P(inline | emph | strong | word)

  def dispAhead[_: P] = P(&("$$"))

  def displayMath[_: P]: P[Div] = P("$$" ~ (CharPred(x => x != '$').rep(1)).! ~ "$$").map{s => DisplayTeX(s)}

  def spanSeq[_: P]: P[List[Span]] =
    (End | P(" ".rep ~ "\n" ~ " ".rep ~ "\n" ~ " ".rep) | dispAhead).map { _ =>
      List()
    } | P(inline ~ " ".rep ~  spanSeq).map { case (x, ys) => x :: ys
       // prepend(x, ys) 
    } |
      P(span ~  spanSeq).map { case (x, ys)               => x :: ys
       // prepend(x, ys) 
    }

  def headHead[_: P]: P[Int] = P("#".rep(min = 1, max = 6).! ~ " ").map {
    _.size
  }

  def para[_: P]: P[Div] = P(spanSeq).map(Paragraph(_))

  def heading[_: P]: P[Div] = P(headHead ~ spanSeq).map {
    case (l, s) => Heading(s, l)
  }

  def div[_: P]: P[Div] = P(displayMath | heading | para)

  def divSeq[_: P]: P[List[Div]] =
    (End).map { _ =>
      List()
    } | P(div ~ divSeq).map { case (x, ys) => x :: ys }
}
