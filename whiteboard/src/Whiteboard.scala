package whiteboard


import scala.scalajs.js.annotation._
import org.scalajs.dom
import scalatags.JsDom.all._
import scala.util._

import scalatags.JsDom.all._

import org.scalajs.dom.html._

@JSExportTopLevel("Whiteboard")
object Whiteboard{
    @JSExport
    def load() : Unit = {
        val jsDiv = dom.document.querySelector("#js-div")
        jsDiv.appendChild(p("Just a hello").render)
        val d = div().render
        d.innerHTML = Content.example.view.toString()
        jsDiv.appendChild(Content.example.view)
    }
}