//
// Scaled XML Mode - a Scaled major mode for editing XML files
// http://github.com/scaled/xml-mode/blob/master/LICENSE

package scaled.xml

import scaled._
import scaled.code.Indenter
import scaled.grammar._
import scaled.code.{CodeConfig, Commenter}

@Plugin(tag="textmate-grammar")
class XmlGrammarPlugin extends GrammarPlugin {
  import EditorConfig._
  import CodeConfig._

  override def grammars = Map("source.xml" -> "XML.ndf")

  override def effacers = List(
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

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string.quoted.single", Syntax.StringLiteral),
    syntaxer("string.quoted.double", Syntax.StringLiteral)
  )
}

@Major(name="xml",
       tags=Array("code", "project", "xml"),
       pats=Array(".*\\.xml", ".*\\.dtml", ".*\\.opml", ".*\\.xsd", ".*\\.tld", ".*\\.jsp",
                  ".*\\.rss", ".*\\.tmLanguage", ".*\\.pom", ".*\\.csproj"),
       desc="A major mode for editing XML files.")
class XmlMode (env :Env) extends GrammarCodeMode(env) {

  override def dispose () :Unit = {} // nada for now

  override def langScope = "source.xml"

  override def createIndenter() = new XmlIndenter(config)

  override val commenter = new Commenter() {
    override def blockOpen   = "<!--"
    override def blockClose  = "-->"
  }
}
