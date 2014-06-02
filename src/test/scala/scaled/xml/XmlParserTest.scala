//
// Scaled XML Mode - a Scaled major mode for editing XML files
// http://github.com/scaled/xml-mode/blob/master/LICENSE

package scaled.xml

import org.junit.Assert._
import org.junit._

class XmlParserTest {
  import XmlParser._

  @Test def testSimple () {
    val xml = "<foo><bar>Baz bingle</bar></foo>"
    assertEquals(List(Open("foo"), Open("bar"), Close("bar"), Close("foo")),
                 XmlParser.parse(xml))
  }

  @Test def testSkipAttrs () {
    val xml = """
    <foo bing="bang">
      <bar bonk="woo, fake tags! <hah> <lolz!>">Baz bingle</bar>
    </foo>"""
    assertEquals(List(Open("foo"), Open("bar"), Close("bar"), Close("foo")),
                 XmlParser.parse(xml))
  }

  @Test def testProlog () {
    val xml = """<?xml version="1.0" encoding="UTF-8"?>"""
    assertEquals(List(Proc("xml")), XmlParser.parse(xml))
  }

  @Test def testComment () {
    val xml = """<!-- we love comments! --> <and>pants!</and> <!-- and more comments -->"""
    assertEquals(List(Open("and"), Close("and")), XmlParser.parse(xml))
  }
}
