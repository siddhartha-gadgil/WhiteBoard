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
    // jsDiv.innerHTML = ""
    // jsDiv.appendChild(d)
    // console.log("updating")

    val selection = dom.window.getSelection()

    // console.log(Try(focusElem.tagName))

    val selected = selection.getRangeAt(0).startContainer.parentNode

    // console.log(selected)

    // console.log(selection.focusOffset)

    // console.log(Try(selected.asInstanceOf[dom.Element].classList.add("focussed") ))

    // console.log(edNode.tagName)

    val text = fullText(edNode)

    log(text)

    // console.log(fullText(edNode))

    // console.log(baseNodes(d).find { n => selected == n })

    val offset = globalOffset(
      baseNodes(edNode),
      selected.asInstanceOf[HTMLElement],
      selection.focusOffset
    )

    // console.log(offset.toString())

    lazy val reparse = fastparse.parse(text, Content.bdy(_))

    reparse.fold[Unit](
      {case f:  (String, Int, Parsed.Extra) => console.log(s"could not parse: \n\nError: $f")},
      {case (newBody : Content.Body, _ :  Int) => 
        console.log(offset)

        def cursor = Content.phraseOffset(newBody.phraseList, offset.get).get

        cursor._1.addCursor(cursor._2)

        // val fNode = cursor._1.view
        // fNode.classList.add("fcs")

        jsDiv.innerHTML = ""
        jsDiv.appendChild(newBody.view)
        jsDiv.appendChild(logDiv)

        newBody.view.oninput = (e) => update()
        // console.log(reparse)
        // console.log("parse above")
        // console.log(
        //   Content.phraseOffset(newBody.phraseList, offset.get).get._1.view
        // )
        // console.log(Content.phraseOffset(newBody.phraseList, offset.get).get._2)

        val nd = dom.document.querySelector(".cursor").asInstanceOf[HTMLElement]
        // console.log(nd)

        // selection.collapse(cursor._1.view, cursor._2)
        val range = dom.document.createRange()
        // console.log(range)
        range.setStart(nd, 0)
        // console.log("set range")
        range.collapse(true)
        val sel = dom.window.getSelection()
        sel.removeAllRanges()
        sel.addRange(range)
        // console.log("focussed")

      }
    )
    // focus.asInstanceOf[dom.Element].classList.contains("tex-inline")

    // d.replaceChild(saves.head, saves.head)

    // log(Content.getXML(s"<div>$edContent</div>").toString)
  }

}
