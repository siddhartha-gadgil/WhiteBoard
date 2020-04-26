package whiteboard

import fastparse._, NoWhitespace._

import scalatags.JsDom.all._

import org.scalajs.dom.html.{Option => _, _}

import scalajs.js

import scalajs.js.Dynamic.{global => g}
import scalatags.JsDom.TypedTag
// import org.xml.sax.InputSource
// import java.io.StringReader
import scala.xml._
import org.scalajs.dom.raw.HTMLElement
import scala.util._
import org.scalajs.dom
import dom.console

sealed trait Content {
  val view: Element
}

sealed trait Phrase extends Content {
  val view: Element

  val sourceLength: Int

  def addCursor(n: Int): Unit
} // span

sealed trait Sentence extends Content {
  val spans: Vector[Phrase]
} // div

object Content {
  def polyDiv(ss: Vector[Element]): TypedTag[Div] = ss match {
    case head +: Vector() => div(contenteditable := true, `class`:= "border border-primary", id := "editor")(head)
    case init :+ last     => polyDiv(init)(last)
    case Vector()         => div(contenteditable := true, `class`:= "border border-primary", id := "editor")
  }

  case class Body(divs: Vector[Sentence]) extends Content {
    lazy val view: org.scalajs.dom.html.Element =
      div(height := "1000")(polyDiv(divs.map(_.view))).render

    lazy val phraseList: Vector[Phrase] = divs.flatMap(_.spans)
  }

  case class Text(body: String) extends Phrase {
    lazy val view: org.scalajs.dom.html.Span = span(body).render

    val sourceLength: Int = body.size

    def addCursor(n: Int): Unit = {
      view.innerHTML = ""
      view.appendChild(
        span(
          span(body.take(n)),
          span(contenteditable := true, `class` := "cursor"),
          span(body.drop(n))
        ).render
      )
    }
  }

  case class Strong(body: String, var formatted: Boolean) extends Phrase {
    lazy val view = span(strong(body)).render

    def simplify(): Unit = {
      formatted = false
      view.innerHTML = ""
      view.appendChild(
        span(
          "__", body, "__"
        ).render
      )
    }

    // view.onclick = (_) => simplify()
    view.oninput = (_) => simplify()

    def addCursor(n: Int): Unit = {
      val m = if (formatted) n else n - 2
      view.innerHTML = ""
      view.appendChild(
        span(
          span("__", body.take(m)),
          span(contenteditable := true, `class` := "cursor"),
          span(body.drop(m), "__")
        ).render
      )
    }

    val sourceLength: Int = body.size + 4
  }

  case class Emph(body: String, var formatted: Boolean) extends Phrase {
    lazy val view: org.scalajs.dom.html.Element = span(em(body)).render

    def simplify(): Unit = {
      formatted = false
      view.innerHTML = ""
      view.appendChild(
        span(
          "_", body, "_"
        ).render
      )
    }

    // view.onclick = (_) => simplify()
    view.oninput = (_) => simplify()

    val sourceLength: Int = body.size + 2

    def addCursor(n: Int): Unit = {
      val m = if (formatted) n else n - 1
      view.innerHTML = ""
      view.appendChild(
        span(
          span("_", body.take(m)),
          span(contenteditable := true, `class` := "cursor"),
          span(body.drop(m), "_")
        ).render
      )
    }
  }

  def polySpan(ss: Vector[Element]): TypedTag[Span] = ss match {
    case head +: Vector() => span(head)
    case init :+ last     => polySpan(init)(last)
    case Vector()         => span()
  }

  case class Heading(spans: Vector[Phrase], level: Int, var formatted: Boolean) extends Sentence {
    lazy val view: org.scalajs.dom.html.Element = level match {
      case 1 => span(h1(polySpan(spans.map(_.view)))).render
      case 2 => span(h2(polySpan(spans.map(_.view)))).render
      case 3 => span(h3(polySpan(spans.map(_.view)))).render
      case 4 => span(h4(polySpan(spans.map(_.view)))).render
      case 5 => span(h5(polySpan(spans.map(_.view)))).render
      case 6 => span(h6(polySpan(spans.map(_.view)))).render
    }

    def simplify(): Unit = {
      formatted = false
      view.innerHTML = ""
      view.appendChild(
        span(
          span("#" * level, " "),
          polySpan(spans.map(_.view))
        ).render
      )
    }

    // view.onclick = (_) => simplify()
    view.oninput = (_) => simplify()

  }

  case class InlineTeX(code: String, var formatted: Boolean) extends Phrase {
    val sourceLength: Int = code.size + 2
    lazy val view: org.scalajs.dom.html.Element = {
      val s =
        span(`class` := "texed inline-tex", attr("data-tex") := code).render
      s.innerHTML =
        if (formatted)
          Try { g.katex.renderToString(code.replace("\u00a0", " ")).toString() }
            .getOrElse(s"<span>${"$"}$code${"$"}</span>")
        else s"<span>${"$"}$code${"$"}</span>"
      s.onclick = (_) => {
        if (formatted) s.innerHTML = s"<span>${"$"}$code${"$"}</span>"
        formatted = false
        s.classList.remove("texed")

      }
      s
    }

    def addCursor(n: Int): Unit = {
      if (formatted) {
        view.innerHTML = ""
        view.appendChild(
          span(
            span("$", code.take(n)),
            span(contenteditable := true, `class` := "cursor"),
            span(code.drop(n), "$")
          ).render
        )
        formatted = false
        view.classList.remove("texed")
      }
    }
  }

  case class DisplayTeX(code: String, var formatted: Boolean) extends Phrase {
    val sourceLength: Int = code.size + 4
    lazy val view: org.scalajs.dom.html.Element = {
      val s =
        span(`class` := "dtexed display-tex", attr("data-tex") := code).render
      s.innerHTML =
        if (formatted)
          Try {
            g.katex
              .renderToString(
                code.replace("\u00a0", " "),
                js.Dynamic.literal("displayMode" -> true)
              )
              .toString()
          }.getOrElse(s"<span>${"$$"}$code${"$$"}</span>")
        else s"<span>${"$$"}$code${"$$"}</span>"
      s.onclick = (_) => {
        if (formatted) s.innerHTML = s"<span>${"$$"}$code${"$$"}</span>"
        formatted = false
        s.classList.remove("dtexed")

      }
      s
    }

    def addCursor(n: Int): Unit = {
      if (formatted) {
        view.innerHTML = ""
        view.appendChild(
          span(
            span("$$", code.take(n)),
            span(contenteditable := true, `class` := "cursor"),
            span(code.drop(n), "$$")
          ).render
        )
        formatted = false
        view.classList.remove("dtexed")
      }
    }
  }

  case class Paragraph(spans: Vector[Phrase]) extends Sentence {
    lazy val view: org.scalajs.dom.html.Element =
      p(polySpan(spans.map(_.view))).render
  }

  val eg = Paragraph(
    Vector(
      Text("Something like"),
      InlineTeX("x^2 + y^2", true),
      Text(" is a formula")
    )
  )

  def inlineTeX[_: P]: P[Phrase] =
    P(" ".rep ~ "$" ~ (CharPred(x => x != '$').rep(1)).! ~ "$").map(s =>
      InlineTeX(s, true)
    )

  def blankLine[_: P]: P[Unit] = P("\n" ~ (" ".rep ~ "\n").rep(1))

  def letter[_: P]: P[String] =
    !blankLine ~ CharPred(x => !Set('$', '_').contains(x)).! //.map(s => Text(s.toString()))

  def word[_: P]: P[Phrase] = letter.rep(1).map(l => Text(l.mkString("")))

  def bold[_: P]: P[Phrase] =
    P("__" ~ letter.rep(1) ~ "__").map(l => Strong(l.mkString(""), true))

  def ital[_: P]: P[Phrase] =
    P("_" ~ letter.rep(1) ~ "_").map(l => Emph(l.mkString(""), true))

  def phrase[_: P]: P[Phrase] = P(displayMath | inlineTeX | bold | ital | word)

  def dispAhead[_: P] = P(&("$$"))

  def displayMath[_: P]: P[Phrase] =
    P("$$" ~ (CharPred(x => x != '$').rep(1)).! ~ "$$").map { s =>
      DisplayTeX(s, true)
    }

  def spanSeq[_: P]: P[Vector[Phrase]] =
    (End | blankLine).map { _ =>
      Vector()
    } | P(inlineTeX ~ spanSeq).map {
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
    case (l, s) => Heading(s, l, true)
  }

  def sentence[_: P]: P[Sentence] = P(heading | para)

  def divSeq[_: P]: P[Vector[Sentence]] =
    (End).map { _ =>
      Vector()
    } | P(sentence ~ divSeq).map { case (x, ys) => x +: ys }

  def bdy[_: P]: P[Body] = divSeq.map(v => Body(v))

  val initialText = 
  """I wrote this minimal whiteboard for the sake of only one feature - parsing a latex formula such as $x+ y$ on the fly. 
  |There are a couple of other features, we can use _emphasis_ or be __strong__ and handle Display text.
  |
  |### Anything else?
  |
  | As you see there are headings (following markdown). We can also display equations such as $$x^2 + y^2 = 1.$$
  |
  |__Note:__ As usual a newline is like a space and a _blank_ line marks a new paragraph.
  | 
  |### But how?
  |
  | To edit a formula, click on it. Look at the source below, or read markdown documentation. You can also experiment here. 
  |Note that you have to move the cursor manually out of formulas and other special environments.
  |
  |### Any improvements due?
  | 
  | The most important one planned is crude figures/handwriting support. This should come in a couple of days. 
  | Other stuff is planned too,
  | and suggestions are welcome.
  |
  """.stripMargin

  lazy val example = parse(
    initialText,
    bdy(_)
  ).get.value

  def phraseOffset(
      phrases: Vector[Phrase],
      offset: Int
  ): Option[(Phrase, Int)] = phrases match {
    case Vector() => None
    case x +: Vector() =>
      if (x.sourceLength >= offset) Some((x, offset))
      else None
    case x +: ys =>
      if (x.sourceLength > offset) Some((x, offset))
      else phraseOffset(ys, offset - x.sourceLength)
  }

  def divOffset(divs: Vector[Sentence], offset: Int): Option[(Phrase, Int)] =
    divs match {
      case Vector() => None
      case x +: ys =>
        phraseOffset(x.spans, offset).orElse {
          val remaining = offset - x.spans.map(_.sourceLength).sum
          divOffset(ys, remaining)
        }
    }
}
