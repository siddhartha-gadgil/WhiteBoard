package whiteboard


import scala.scalajs.js.annotation._
import org.scalajs.dom
import scalatags.JsDom.all._
import scala.util._

import scalatags.JsDom.all._

import org.scalajs.dom.html._
import scala.util._
import org.scalajs.dom.raw.HTMLElement

@JSExportTopLevel("Whiteboard")
object Whiteboard{
    val jsDiv = dom.document.querySelector("#js-div")

    val logDiv = div().render

    def log(s: String) : Unit= {
        logDiv.innerHTML = ""
        logDiv.appendChild(pre(s).render)
    }

    lazy val d = Content.example.view

    def edNode =
        dom.document.querySelector("#editor").asInstanceOf[HTMLElement]

    def children(node: HTMLElement) : Vector[HTMLElement] = 
         {
            val c = node.childNodes
            (0 until(c.length)).toVector.map(i => c(i))
        }.collect{case n : HTMLElement if n != () => n}

    def tagPad(s: String, tag: String) = tag match {
        case "P" => s+"\n\n"
        case "STRONG" => "__"+s+"__"
        case "EM" => "_"+s+"_"
        case s"H$n" => 
            val level = n.toInt
            ("#" * level) + " " + s+ "\n\n"
        case _ => s
    }

    def fullText(node: HTMLElement) : String = 
        if (node.classList.contains("inline-tex")) "$"+node.attributes.getNamedItem("data-tex").value+"$"
        else if (node.classList.contains("display-tex")) "$$"+node.attributes.getNamedItem("data-tex").value+"$$"
        else {
            val cs = children(node)
            if (cs.isEmpty) tagPad(node.textContent, node.tagName) else tagPad(cs.map(fullText(_)).mkString(""), node.tagName)
        }

    def offspring(node: dom.Node) : Vector[HTMLElement] = 
        (if (node.hasChildNodes()) {
            val c = node.childNodes
            node +: (0 until(c.length)).toVector.flatMap(i => offspring(c(i)))
        }
        else Vector(node)).collect{case n : HTMLElement if n != () => n}

    @JSExport
    def load() : Unit = {
            
        // d.onpointermove = (e) => update()
        d.oninput = (e) => update()
        d.onclick = (e) => update()
        jsDiv.appendChild(d)
        jsDiv.appendChild(logDiv)
        
    }

    def update() : Unit = {
        // jsDiv.innerHTML = ""
        // jsDiv.appendChild(d)
        dom.console.log("updating")

        val savedRaw = d.childNodes

        val saves  = (0 until savedRaw.length).map(j =>  savedRaw(j))

        val selection = dom.window.getSelection()

        val focusOpt  = Option(dom.window.getSelection()).map(_.focusNode)

        val focusElem = focusOpt.map{_.asInstanceOf[dom.raw.HTMLElement]}.get

        // focusElemOpt.foreach(_.classList.add("focussed"))

        focusOpt.foreach{e => dom.console.log(e); dom.console.log(e.nodeValue); dom.console.log(Try(logDiv.setAttribute("blah", "blah") ).toString )}      

        dom.console.log("got an element") 

        // dom.console.log(Try(focusElem.tagName))
        

        val selected = selection.getRangeAt(0).startContainer.parentNode

        dom.console.log(selected)

        dom.console.log(Try(selected.asInstanceOf[dom.Element].classList.add("focussed") ))

        dom.console.log(edNode.tagName)

        log(fullText(edNode))

        dom.console.log(fullText(edNode))

        // dom.console.log( offspring(d).find{n => focusOpt == Some(n)})

        // focus.asInstanceOf[dom.Element].classList.contains("tex-inline")

        // d.replaceChild(saves.head, saves.head)

        // log(Content.getXML(s"<div>$edContent</div>").toString)
    }


}