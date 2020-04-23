package whiteboard


import scala.scalajs.js.annotation._
import org.scalajs.dom
import scalatags.JsDom.all._
import scala.util._

import scalatags.JsDom.all._

import org.scalajs.dom.html._

@JSExportTopLevel("Whiteboard")
object Whiteboard{
    val jsDiv = dom.document.querySelector("#js-div")

    lazy val d = Content.example.view

    @JSExport
    def load() : Unit = {
        
        
        d.oninput = (e) => update()
        jsDiv.appendChild(d)
    }

    def update() : Unit = {
        // jsDiv.innerHTML = ""
        // jsDiv.appendChild(d)

        val savedRaw = d.childNodes

        val saves  = (0 until savedRaw.length).map(j =>  savedRaw(j))

        val focus = dom.window.getSelection().focusNode

        // d.replaceChild(saves.head, saves.head)

        jsDiv.appendChild(div(focus.textContent).render)
    }


}