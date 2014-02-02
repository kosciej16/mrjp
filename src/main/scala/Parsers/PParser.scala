package PParser

import scala.util.parsing.combinator.syntactical._
import Ptypes._
import Stypes._
import SParser.StmtParser
import EParser.ExprParser
import TParser.TypeParser
import CParser.CommonParser

object ProgramParser extends StandardTokenParsers with StmtParser
    with TypeParser with ExprParser with CommonParser{
//    lexical.delimiters ++= List("{","}")

    lexical.delimiters ++= List(";","=",",","(",")","+","-", "*", "/", "{", "}", "(", ")", "%")
    lexical.delimiters ++= List("++","--",">","<","==",">=","<=","!=", "!", "&&", "||")
    lexical.reserved.add("true")
    lexical.reserved.add("false")
    lexical.reserved.add("string")
    lexical.reserved.add("int")
    lexical.reserved.add("boolean")
    lexical.reserved.add("void")
    lexical.reserved.add("return")
    lexical.reserved.add("while")
    lexical.reserved.add("if")
    lexical.reserved.add("else")

    def typ = getTypeParser()
    def expr = getExprParser()
    def stmt = getStmtParser()
    def block : Parser[SBlock]= "{" ~ rep(stmt) ~ "}" ^^ { case _ ~ slist ~ _ => SBlock(slist) }

    def program = rep(fnDef) 
    def fnDef = typ ~ item ~ "(" ~ repsep(arg,",") ~ ")" ~ block ^^ { case t ~ i ~ _ ~ params ~ _ ~ b => PFnDef(t, i, params, b) }
    def arg = typ ~ item ^^ { case t ~ i => PArg(t,i) } 


    def parse(s:String) = {
      val token = new lexical.Scanner(s)
      phrase(program)(token)
    }

    def test(s:String) = {
      parse(s) match {
        case Success(tree, _) =>
          println("Tree: "+tree)
        case e: NoSuccess => Console.err.println(e)
      }
    }

//    def main(args:Array[String]) = test(args(0))
}
