//
// Scaled XML Mode - a Scaled major mode for editing XML files
// http://github.com/scaled/xml-mode/blob/master/LICENSE

package scaled.xml

import org.junit.Assert._
import org.junit._
import scaled.TextStore
import scaled.grammar._
import scaled.impl.BufferImpl

class XmlTest {

  val testXMLCode = Seq(
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

  val xml = Grammar.parseNDF(getClass.getClassLoader.getResource("XML.ndf"))
  val grammars = List(xml)

  @Test def debugGrammar () {
    // xml.print(System.out)
    xml.scopeNames foreach println

    // val buffer = BufferImpl(new TextStore("Test.xml", "", testXMLCode))
    // val scoper = new Scoper(grammars, buffer, Nil)
    // println(scoper.showMatchers(Set("#internalSubset", "#tagStuff", "#entity")))
  }
}
