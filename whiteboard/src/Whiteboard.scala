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
        val d = Content.example.view
        d.oninput = (e) => jsDiv.appendChild(p("You clicked").render)
        jsDiv.appendChild(d)
    }
}