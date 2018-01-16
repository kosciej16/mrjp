package TParser

import scala.util.parsing.combinator.syntactical._
import Types._

trait TypeParser extends StandardTokenParsers {
//    lexical.delimiters ++= List("{","}")


    def getTypeParser () : Parser[Type] = {

      def typ = ("int" | "string" | "boolean" | "void" ) ^^ {s => TType(s)}
      def array : Parser[Type] = (typ | struct) ~ "[" ~ "]" ^^ { case t ~ _ ~ _ => TArray(t) }
      def struct : Parser[Type] = "struct" ~ ident ^^ { case _ ~ i => TStruct(i) }
      def fun = typ ~ "(" ~ rep(typ) ~ ")" ^^ { case t ~ _ ~ list ~ _ => TFun(t, list) }
      def typeParser = array | typ | fun | struct
      return typeParser
    }

}
