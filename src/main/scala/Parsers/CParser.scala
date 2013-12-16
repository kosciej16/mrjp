package CParser
import scala.util.parsing.combinator.syntactical._

import Ctypes._

trait CommonParser extends StandardTokenParsers {

  def item : Parser[CIdent] = ident ^^ { s => CIdent(s) }
}
