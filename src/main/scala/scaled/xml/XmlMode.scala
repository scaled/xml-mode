//
// Scaled XML Mode - a Scaled major mode for editing XML files
// http://github.com/scaled/xml-mode/blob/master/LICENSE

package scaled.xml

import scaled._
import scaled.grammar._
import scaled.major.CodeConfig
import scaled.util.Commenter

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

  def xmlGrammar = Grammar.parse(stream("XML.tmLanguage"))
  lazy val grammars = Seq(xmlGrammar)
}

@Major(name="xml",
       tags=Array("project", "xml"),
       pats=Array(".*\\.xml", ".*\\.tmLanguage"),
       desc="A major mode for editing XML files.")
class XmlMode (env :Env) extends GrammarCodeMode(env) {

  override def dispose () {} // nada for now

  override def configDefs = XmlConfig :: super.configDefs
  override def grammars = XmlConfig.grammars
  override def effacers = XmlConfig.effacers

  override val indenters = Nil
  override val commenter = new Commenter(buffer)
}
