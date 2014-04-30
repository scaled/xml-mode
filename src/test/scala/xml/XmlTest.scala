//
// Scaled XML Mode - a Scaled major mode for editing XML files
// http://github.com/scaled/xml-mode/blob/master/LICENSE

package xml

import java.io.File
import java.io.StringReader
import org.junit.Assert._
import org.junit._
import scaled.grammar._
import scaled.impl.BufferImpl

class XmlTest {

  val textXMLCode = Seq(
    //                1         2         3
    //      0123456789012345678901234567890
    /* 0*/ "<xml>",
    /* 1*/ "",
    /* 2*/ "<foo>",
    /* 3*/ " <!- a comment, how lovely -->",
    /* 4*/ " <bar>baz</bar>",
    /* 5*/ " <dingle a=\"b\" />",
    /* 6*/ "</foo>",
    /* 7*/ "}").mkString("\n")

  val xml = Grammar.parse(getClass.getClassLoader.getResourceAsStream("XML.tmLanguage"))
  val grammars = List(xml)

  def testBuffer (name :String, text :String) =
    BufferImpl(name, new File(name), new StringReader(text))

  @Test def debugGrammar () {
    // xml.print(System.out)
    xml.scopeNames foreach println

    // val buffer = testBuffer("Test.xml", textXMLCode)
    // val scoper = new Scoper(grammars, buffer, Nil)
    // println(scoper.showMatchers(Set("#internalSubset", "#tagStuff", "#entity")))
  }
}
