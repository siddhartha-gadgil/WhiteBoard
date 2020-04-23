import mill._, scalalib._, scalajslib._, define.Task
// import ammonite.ops._

object whiteboard extends ScalaJSModule{
    def scalaVersion = "2.13.1"
    def scalaJSVersion = "1.0.1"
  
    def platformSegment = "js"
  
    import coursier.maven.MavenRepository
  
    def repositories = super.repositories ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/releases")
    )
  
    def ivyDeps = Agg(
      ivy"org.scala-js::scalajs-dom::1.0.0",
      // ivy"in.nvilla::monadic-html::0.4.0",
      ivy"com.lihaoyi::scalatags::0.9.0",
      ivy"com.lihaoyi::fastparse::2.3.0",
      ivy"org.scala-lang.modules::scala-xml:1.2.0"
    )
  
    def pack(): define.Command[PathRef] = T.command {
      def js = fastOpt()
      val target = os.pwd/ "docs" / "js" / "whiteboard.js"
    //   pprint.log(target)
      os.copy.over(js.path, target, createFolders = true)
      js
    }
  
  
  }