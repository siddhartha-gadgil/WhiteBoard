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

  val logDiv = div().render

  def log(s: String): Unit = {
    logDiv.innerHTML = ""
    logDiv.appendChild(pre(s).render)
  }

  lazy val d = Content.example.view

  def edNode =
    dom.document.querySelector("#editor").asInstanceOf[HTMLElement]

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
      else globalOffset(ys, p, offset + fullText(x).size)
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
    jsDiv.appendChild(logDiv)

  }

  def update(): Unit = {
    val selection = dom.window.getSelection()

    val text = fullText(edNode)

    log(text)

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

          offset.foreach{n => lastPosition = n}

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

          offset.fold[Unit] {
            // jsDiv.innerHTML = ""
            // jsDiv.appendChild(newBody.view)
            // jsDiv.appendChild(logDiv)
            // newBody.view.oninput = (e) => update()
            // val range = dom.document.createRange()

            // range.setStart(newBody.view, 0)
            // range.collapse(true)
            // val sel = dom.window.getSelection()
            // sel.removeAllRanges()
            // sel.addRange(range)
          } {
            pos =>
              //   console.log("global offset", pos)

              val cursorOpt = Content.divOffset(newBody.divs, pos)
              console.log(cursorOpt)
              if (cursorOpt.isEmpty) console.log(pos, newBody.phraseList)
              cursorOpt.fold[Unit] {
                jsDiv.innerHTML = ""
                jsDiv.appendChild(newBody.view)
                jsDiv.appendChild(logDiv)

              } {
                cursor =>
                  //   console.log(cursor._1)
                  //   console.log(cursor._1.view)
                  //   console.log(cursor._2)

                  cursor._1.addCursor(cursor._2)

                  newBody.divs.collect{case h:  whiteboard.Content.Heading => h}.foreach{h => if (h.spans.contains(cursor._1)) h.simplify()}

                  jsDiv.innerHTML = ""
                  jsDiv.appendChild(newBody.view)
                  jsDiv.appendChild(logDiv)

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
