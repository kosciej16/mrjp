package TParser

import scala.util.parsing.combinator.syntactical._
import Types._

trait TypeParser extends StandardTokenParsers {
//    lexical.delimiters ++= List("{","}")


    def getTypeParser () : Parser[Type] = {

      def typ = ("int" | "string" | "boolean" | "void") ^^ {s => TType(s)}
      def fun = typ ~ "(" ~ rep(typ) ~ ")" ^^ { case t ~ _ ~ list ~ _ => TFun(t, list) }
      def typeParser = typ | fun
      return typeParser
    }

}
