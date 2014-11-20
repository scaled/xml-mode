//
// Scaled XML Mode - a Scaled major mode for editing XML files
// http://github.com/scaled/xml-mode/blob/master/LICENSE

package scaled.xml

import scaled._
import scaled.code.Indenter
import scaled.grammar._
import scaled.code.{CodeConfig, Commenter}

object XmlConfig extends Config.Defs {
  import EditorConfig._
  import CodeConfig._
  import GrammarConfig._

  // maps TextMate grammar scopes to Scaled style definitions
  val effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", warnStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name.tag", functionStyle),
    effacer("entity.other", variableStyle),

    effacer("variable.language.documentroot", preprocessorStyle),
    effacer("variable.language.entity", typeStyle)
  )

  val grammars = resource("XML.ndf")(Grammar.parseNDFs)
}

@Major(name="xml",
       tags=Array("code", "project", "xml"),
       pats=Array(".*\\.xml", ".*\\.dtml", ".*\\.opml", ".*\\.xsd", ".*\\.tld", ".*\\.jsp",
                  ".*\\.rss", ".*\\.tmLanguage"),
       desc="A major mode for editing XML files.")
class XmlMode (env :Env) extends GrammarCodeMode(env) {

  override def dispose () {} // nada for now

  override def configDefs = XmlConfig :: super.configDefs
  override def grammars = XmlConfig.grammars.get
  override def effacers = XmlConfig.effacers

  override def createIndenter() = new XmlIndenter(buffer, config)
  override val commenter = new Commenter()
}
