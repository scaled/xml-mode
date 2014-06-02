//
// Scaled XML Mode - a Scaled major mode for editing XML files
// http://github.com/scaled/xml-mode/blob/master/LICENSE

package scaled.xml

import scaled._
import scaled.code.{CodeConfig, Block, Indenter}
import scaled.util.Chars

object XmlIndenter {
  import Indenter._
  import Chars._

  class CloseTag (ctx :Context) extends Indenter(ctx) {
    def apply (block :Block, line :LineV, pos :Loc) :Option[Int] = {
      val tags = XmlParser.parse(line)
      if (tags.isEmpty || !tags.head.isClose) None
      // we're looking at a close tag, indent it to its matching open tag
      else {
        val name = tags.head.name ; val nameM = Matcher.exact(name)
        // check every line preceding this one that contains our tag name for the matching open tag
        def seek (last :Loc) :Int = buffer.findBackward(nameM, last.atCol(0)) match {
          case Loc.None =>
            debug(s"Failed to find <$name> to match indent.")
            readIndent(buffer, pos)
          case open =>
            XmlParser.parse(buffer.line(open)).find(t => t.name == name && t.isOpen) match {
              case None => seek(open) // keep looking
              case Some(t) =>
                debug(s"Matching indent of <$name> @ $open")
                readIndent(buffer, open)
            }
        }
        Some(seek(pos))
      }
    }
  }

  /** Indents nested tags relative to their enclosing tag. */
  class NestedTag (ctx :Context) extends Indenter(ctx) {
    def apply (block :Block, line :LineV, pos :Loc) :Option[Int] = {
      var open = Loc.None ; var close = Loc.None
      // scan backwards to find the first XML tag preceding pos
      def seek (row :Int) :Int =
        if (row < 0) { debug(s"Hit start of buffer seeking open/close tag.") ; 0 }
        else {
          val line = buffer.line(row)
          var tags = XmlParser.parse(line)
          var tt = tags.length - 1 ; while (tt >= 0) {
            val tag = tags(tt)
            if (tag.isOpen) {
              debug(s"Indenting from <open> @ $row")
              return indentFrom(readIndent(line), 1)
            }
            else if (tag.isClose) {
              debug(s"Matching indent of </close> @ $row")
              return readIndent(line)
            }
            tt -= 1
          }
          seek(row-1)
        }
      Some(seek(pos.row-1))
    }
  }
}
