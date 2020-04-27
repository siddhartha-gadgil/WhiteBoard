package whiteboard

import scala.scalajs.js.annotation._
import org.scalajs.dom
import scalatags.JsDom.all._
import scala.util._

import scalatags.JsDom.all._

import org.scalajs.dom.html.{Option => _, _}
import scala.util._
import org.scalajs.dom.raw.HTMLElement

import dom.console
import scalajs.js.Dynamic.{global => g}
import fastparse._
@JSExportTopLevel("Whiteboard")
object Whiteboard {
  val jsDiv = dom.document.querySelector("#js-div")

  var lastPosition: Int = 0

  val sourceDiv = div(`class` := "border border-success source extra").render

  def showSource(s: String): Unit = {
    sourceDiv.innerHTML = ""
    sourceDiv.appendChild(pre(s).render)
  }

  showSource(Content.initialText)

  def edNode =
    dom.document.querySelector("#editor").asInstanceOf[HTMLElement]

  var focussed = false

  def focus(): Unit = {
    focussed = true
    val extras = dom.document.querySelectorAll(".extra")
    for { j <- 0 until (extras.length) } extras(j)
      .asInstanceOf[HTMLElement]
      .classList
      .add("extra-hide")
    edNode.classList
      .remove("editor-bounded")
    edNode.classList
      .remove("border")
    edNode.classList
      .remove("border-primary")
    jsDiv.asInstanceOf[HTMLElement].ondblclick = (_) => unFocus()
  }

  def unFocus(): Unit = {
    val extras = dom.document.querySelectorAll(".extra")
    for { j <- 0 until (extras.length) } extras(j)
      .asInstanceOf[HTMLElement]
      .classList
      .remove("extra-hide")
    edNode.classList
      .add("editor-bounded")
    edNode.classList
      .add("border")
    edNode.classList
      .add("border-primary")

    jsDiv.asInstanceOf[HTMLElement].ondblclick = (_) => focus()
    focussed = false
  }

  jsDiv.asInstanceOf[HTMLElement].ondblclick = (_) => focus()

  lazy val d = Content.example.view

  def children(node: HTMLElement): Vector[HTMLElement] = {
    val c = node.childNodes
    (0 until (c.length)).toVector.map(i => c(i))
  }.collect { case n: HTMLElement if n != () => n }

  def tagPad(s: String, tag: String) = tag match {
    case "P"      => s + "\n\n"
    case "STRONG" => "__" + s + "__"
    case "EM"     => "_" + s + "_"
    case s"H$n" =>
      val level = n.toInt
      ("#" * level) + " " + s + "\n\n"
    case _ => s
  }

  def fullText(node: HTMLElement): String =
    if (node.classList.contains("texed"))
      "$" + node.attributes.getNamedItem("data-tex").value + "$"
    else if (node.classList.contains("dtexed"))
      "$$" + node.attributes.getNamedItem("data-tex").value + "$$"
    else {
      val cs = children(node)
      if (cs.isEmpty) tagPad(node.textContent, node.tagName)
      else tagPad(cs.map(fullText(_)).mkString(""), node.tagName)
    }

  def baseNodes(node: HTMLElement): Vector[HTMLElement] =
    if (node.classList.contains("texed")) Vector(node)
    else if (node.classList.contains("dtexed")) Vector(node)
    else if (node.classList.contains("padding")) Vector()
    else {
      val cs = children(node)
      if (cs.isEmpty) Vector(node) else cs.flatMap(baseNodes(_))
    }

  def globalOffset(
      ns: Vector[HTMLElement],
      p: HTMLElement,
      offset: Int
  ): Option[Int] = ns match {
    case Vector() => None
    case x +: ys =>
      if (x == p) Some(offset)
      else {
          val shift = if (p.classList.contains("blank")) 0 else fullText(x).size
          globalOffset(ys, p, offset + shift)
      }
    case _ => None
  }

  def offspring(node: dom.Node): Vector[HTMLElement] =
    (if (node.hasChildNodes()) {
       val c = node.childNodes
       node +: (0 until (c.length)).toVector.flatMap(i => offspring(c(i)))
     } else Vector(node)).collect { case n: HTMLElement if n != () => n }

  @JSExport
  def load(): Unit = {

    // d.onpointermove = (e) => update()
    d.oninput = (e) => update()
    // d.onclick = (e) => update()
    jsDiv.appendChild(d)
    jsDiv.appendChild(
      div(p(), h3(`class` := "extra")("Source"), sourceDiv).render
    )

  }

  def update(): Unit = {
    val selection = dom.window.getSelection()

    val text = fullText(edNode)

    showSource(text)

    lazy val reparse = fastparse.parse(text, Content.bdy(_))

    reparse.fold[Unit](
      {
        case f: (String, Int, Parsed.Extra) =>
          console.log(s"could not parse: \n\nError: $f")
      }, {
        case (newBody: Content.Body, _: Int) =>
          val selected = selection.getRangeAt(0).startContainer.parentNode

          val basic = baseNodes(edNode)

          val offset = globalOffset(
            basic,
            selected.asInstanceOf[HTMLElement],
            selection.focusOffset
          )

          offset.foreach { n => lastPosition = n }

          if (!basic.contains(selected))
            console.log("missing selected node", selected)

          if (offset.isEmpty)
            console.log(
              (
                baseNodes(edNode),
                selected.asInstanceOf[HTMLElement],
                selection.focusOffset
              )
            )

          offset.foreach {
            pos =>
                console.log("global offset", pos)
                console.log("Selection", selected)
                console.log("local offset", selection.focusOffset)
                console.log("Basic")
                // basic.foreach{n => console.log(n); console.log(fullText(n)); console.log(fullText(n).size)}
              val cursorOpt = Content.divOffset(newBody.divs, pos)
              //   console.log(cursorOpt)
              if (cursorOpt.isEmpty) console.log(pos, newBody.phraseList)
              cursorOpt.fold[Unit] {
                jsDiv.innerHTML = ""
                jsDiv.appendChild(newBody.view)
                jsDiv.appendChild(div(p(), h3(`class` := "extra")("Source"), sourceDiv).render)

              } {
                cursor =>
                  //   console.log(cursor._1)
                  //   console.log(cursor._1.view)
                  //   console.log(cursor._2)

                  cursor._1.addCursor(cursor._2)

                  newBody.divs
                    .collect { case h: whiteboard.Content.Heading => h }
                    .foreach { h =>
                      if (h.spans.contains(cursor._1)) h.simplify()
                    }

                  jsDiv.innerHTML = ""
                  jsDiv.appendChild(newBody.view)
                  jsDiv.appendChild(div(p(), h3(`class` := "extra")("Source"), sourceDiv).render)

                  if (focussed) focus()

                  newBody.view.oninput = (e) => update()

                  val nd =
                    dom.document
                      .querySelector(".cursor")
                      .asInstanceOf[HTMLElement]

                  val range = dom.document.createRange()

                  range.setStart(nd, 0)
                  range.collapse(true)
                  val sel = dom.window.getSelection()
                  sel.removeAllRanges()
                  sel.addRange(range)
                  // console.log("focussed")

              }

          }

      }
    )
    // focus.asInstanceOf[dom.Element].classList.contains("tex-inline")

    // d.replaceChild(saves.head, saves.head)

    // log(Content.getXML(s"<div>$edContent</div>").toString)
  }

}
