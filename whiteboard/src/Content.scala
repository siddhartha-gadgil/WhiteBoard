package whiteboard

import fastparse._, NoWhitespace._

import scalatags.JsDom.all._

import org.scalajs.dom.html._

import scalajs.js.Dynamic.{global => g}
import scalatags.JsDom.TypedTag

sealed trait Content {
  def view: Element
}

sealed trait Phrase extends Content {
  def view: Element
} // span

sealed trait Sentence extends Content // div

object Content {
    def polyDiv(ss: Vector[Element]): TypedTag[Div] = ss match {
    case head +: Vector()     => div(contenteditable := true)(head)
    case init :+ last => polyDiv(init)(last)
    case Vector()     => div(contenteditable := true)
  }

    case class Body(divs: Vector[Sentence]) extends Content{
        def view: org.scalajs.dom.html.Element = 
            p(polyDiv(divs.map(_.view))).render
    }

  case class Text(body: String) extends Phrase {
    def view: org.scalajs.dom.html.Span = span(body).render
  }

  case class Strong(body: String) extends Phrase {
    def view = strong(body).render
  }

  case class Emph(body: String) extends Phrase {
    def view: org.scalajs.dom.html.Element = em(body).render
  }

  def polySpan(ss: Vector[Element]): TypedTag[Span] = ss match {
    case head +: Vector() => span(head)
    case init :+ last => polySpan(init)(last)
    case Vector()     => span()
  }

  case class Heading(spans: Vector[Phrase], level: Int) extends Sentence {
    def view: org.scalajs.dom.html.Element = level match {
      case 1 => h1(polySpan(spans.map(_.view))).render
      case 2 => h2(polySpan(spans.map(_.view))).render
      case 3 => h3(polySpan(spans.map(_.view))).render
      case 4 => h4(polySpan(spans.map(_.view))).render
      case 5 => h5(polySpan(spans.map(_.view))).render
      case 6 => h6(polySpan(spans.map(_.view))).render
    }
  }

  case class InlineTeX(code: String) extends Phrase {
    def view: org.scalajs.dom.html.Element = {
      val s = span().render
      s.innerHTML = g.katex.renderToString(code).toString()
      s
    }
  }

  case class DisplayTeX(code: String) extends Sentence {
    def view: org.scalajs.dom.html.Element = {
      val s = div().render
      s.innerHTML = g.katex.renderToString(code).toString()
      s
    }
  }

  case class Paragraph(spans: Vector[Phrase]) extends Sentence {
    def view: org.scalajs.dom.html.Element = 
        p(polySpan(spans.map(_.view))).render
  }

  val eg = Paragraph(
    Vector(
      Text("Something like"),
      InlineTeX("x^2 + y^2"),
      Text(" is a formula")
    )
  )

  def inline[_: P]: P[Phrase] =
    P(" ".rep ~ "$" ~ (CharPred(x => x != '$').rep(1)).! ~ "$").map(s =>
      InlineTeX(s)
    )

  def blankLine[_: P]: P[Unit] = P(" ".rep ~ "\n").rep(2)

  def letter[_: P]: P[String] =
    !blankLine ~ CharPred(x => !Set('$', '_').contains(x)).! //.map(s => Text(s.toString()))

  def word[_: P]: P[Phrase] = letter.rep(1).map(l => Text(l.mkString("")))

  def bold[_: P]: P[Phrase] =
    P("__" ~ letter.rep(1) ~ "__").map(l => Strong(l.mkString("")))

  def ital[_: P]: P[Phrase] =
    P("_" ~ letter.rep(1) ~ "_").map(l => Emph(l.mkString("")))

  def phrase[_: P]: P[Phrase] = P(inline | ital | bold | word)

  def dispAhead[_: P] = P(&("$$"))

  def displayMath[_: P]: P[Sentence] =
    P("$$" ~ (CharPred(x => x != '$').rep(1)).! ~ "$$").map { s =>
      DisplayTeX(s)
    }

  def spanSeq[_: P]: P[Vector[Phrase]] =
    (End | P(" ".rep ~ "\n" ~ " ".rep ~ "\n" ~ " ".rep) | dispAhead).map { _ =>
      Vector()
    } | P(inline ~ " ".rep ~ spanSeq).map {
      case (x, ys) => x +: ys
      // prepend(x, ys)
    } |
      P(phrase ~ spanSeq).map {
        case (x, ys) => x +: ys
        // prepend(x, ys)
      }

  def headHead[_: P]: P[Int] = P("#".rep(min = 1, max = 6).! ~ " ").map {
    _.size
  }

  def para[_: P]: P[Sentence] = P(spanSeq).map(Paragraph(_))

  def heading[_: P]: P[Sentence] = P(headHead ~ spanSeq).map {
    case (l, s) => Heading(s, l)
  }

  def sentence[_: P]: P[Sentence] = P(displayMath | heading | para)

  def divSeq[_: P]: P[Vector[Sentence]] =
    (End).map { _ =>
      Vector()
    } | P(sentence ~ divSeq).map { case (x, ys) => x +: ys }
}
