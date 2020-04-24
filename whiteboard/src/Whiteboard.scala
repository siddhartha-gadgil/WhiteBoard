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
        logDiv.appendChild(span(s).render)
    }

    lazy val d = Content.example.view

    def edContent : String =
        dom.document.querySelector("#editor").innerHTML

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

        // dom.console.log( offspring(d).find{n => focusOpt == Some(n)})

        // focus.asInstanceOf[dom.Element].classList.contains("tex-inline")

        // d.replaceChild(saves.head, saves.head)

        // log(edContent)
    }


}