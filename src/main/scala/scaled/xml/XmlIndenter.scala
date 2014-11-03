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
    def apply (block :Block, line :LineV, pos :Loc) :Int = {
      val tags = XmlParser.parse(line)
      if (tags.isEmpty || !tags.head.isClose) NA
      // we're looking at a close tag, indent it to its matching open tag
      else {
        val name = tags.head.name ; val nameM = Matcher.exact(name)
        var count = 1
        // check every line preceding this one that contains our tag name for the matching open tag
        def seek (last :Loc) :Int = buffer.findBackward(nameM, last.atCol(0)) match {
          case Loc.None =>
            debug(s"Failed to find <$name> to match indent.")
            readIndent(buffer, pos)
          case open =>
            // scan backwards, skipping close/open pairs, to find the open tag we seek
            for (tag <- XmlParser.parse(buffer.line(open)).filter(_.name == name).reverse) {
              count += (if (tag.isOpen) -1 else 1)
              if (count == 0) {
                debug(s"Matching indent of <$name> @ $open")
                return readIndent(buffer, open)
              }
            }
            seek(open) // didn't find match on this line, keep going
        }
        seek(pos)
      }
    }
  }

  /** Indents nested tags relative to their enclosing tag. */
  class NestedTag (ctx :Context) extends Indenter(ctx) {
    def apply (block :Block, line :LineV, pos :Loc) :Int = {
      // scan backwards to find the first XML tag preceding pos
      @inline @tailrec def seek (row :Int) :Int =
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
      seek(pos.row-1)
    }
  }
}
