//
// Scaled XML Mode - a Scaled major mode for editing XML files
// http://github.com/scaled/xml-mode/blob/master/LICENSE

package scaled.xml

import scala.collection.mutable.ArrayBuffer

/** Very primitive XML parser that converts a line of text into a sequence of open/close tags.
  * Everything in between tags is ignored, attributes are ignored. This parser exists so that we can
  * indent XML without resorting to the use of regular expressions.
  *
  * NOTE: this parser does not (and cannot due to its 'I see only one line at a time' nature) handle
  * CDATA sections. Some day we'll have to bite the bullet and revamp XML indentation to cope with
  * CDATA. Blah.
  */
object XmlParser {

  abstract class Tag {
    def name :String
    def isOpen :Boolean = false
    def isClose :Boolean = false
  }
  case class Open (name :String) extends Tag {
    override def isOpen = true
  }
  case class Close (name :String) extends Tag {
    override def isClose = true
  }
  case class OpenClose (name :String) extends Tag {
    override def isOpen = true
    override def isClose = true
  }
  case class Proc (name :String) extends Tag

  /** Parses the XML tags in `cs` (if any) and returns all matched tags. */
  def parse (cs :CharSequence) :Seq[Tag] = new Parser(cs).parse()

  private class Parser (cs :CharSequence) {
    private val len = cs.length
    private val name = new StringBuilder()
    private val tags = ArrayBuffer[Tag]()

    // state:
    // 0 - between tags
    // 1 - seen < or </ (isClose tracks) or <? (isProc tracks)
    // 2 - parsed name, ignoring rest of tag
    // 3 - in string
    // 4 - in comment
    private var pos = 0
    private var isClose = false
    private var isProc = false

    def parse () = {
      var state = 0
      while (pos < len) {
        val c = cs.charAt(pos)
        pos += 1
        // println(s"step($c, $state)")
        state = step(c, len, state)
      }
      tags
    }

    // this assumes pos is incremented *before* calling step()
    private def peek :Char = if (pos >= len) 0 else cs.charAt(pos)
    private def eat (c :Char) = if (peek != c) false else { pos += 1 ; true }
    private def eat (seq :String) :Boolean = {
      var p = pos ; var s = 0 ; while (s < seq.length) {
        if (p >= len || cs.charAt(p) != seq.charAt(s)) return false
        p += 1 ; s += 1
      }
      pos += seq.length
      true
    }

    private def step (c :Char, len :Int, stepState :Int) :Int = stepState match {
      case 0 =>
        if (c != '<') 0 // keep looking for open tag
        else if (eat("!--")) 4 // transition to "in comment"
        else {
          isClose = eat('/')
          isProc = eat('?')
          1 // transition to "parsing tag name"
        }

      case 1 =>
        if (isNameChar(c)) { name.append(c) ; 1 }
        // otherwise reprocess 'c' in state 2
        else step(c, len, 2)

      case 2 =>
        val autoClose = (c == '/' && eat('>'))
        if (c == '>' || autoClose) {
          // TODO: should we complain about </foo/>? probably not, correctness is not our problem
          tags += (if (isProc) Proc(name.toString)
                   else if (isClose) Close(name.toString)
                   else if (autoClose) OpenClose(name.toString)
                   else Open(name.toString))
          name.clear()
          0 // back to in between tags
        }
        else if (c == '"') 3
        else 2

      case 3 =>
        if (c == '"') 2 else 3

      case 4 =>
        if (c == '-' && eat("->")) 0 // back to between tags
        else 4
    }
  }

  private def isNameChar (c :Char) = {
    def in (l :Char, u :Char) = l <= c && c <= u
    c != ' ' && (
      in('a', 'z') || in('A', 'Z') || in('0', '9') || c == ':' || c == '_' || c == '-' ||
      c == '.' || c == '\u00B7' || in('\u00C0', '\u00D6') || in('\u00D8', '\u00F6') ||
      in('\u00F8', '\u02FF') || in('\u0370', '\u037D') || in('\u037F', '\u1FFF') ||
      in('\u200C', '\u200D') || in('\u2070', '\u218F') || in('\u2C00', '\u2FEF') ||
      in('\u3001', '\uD7FF') || in('\uF900', '\uFDCF') || in('\uFDF0', '\uFFFD') ||
      in('\u0300', '\u036F') || in('\u203F', '\u2040') /*|| in('\u10000', '\uEFFFF')*/
    )
  }
}
