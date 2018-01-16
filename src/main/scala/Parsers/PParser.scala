package PParser

import util.parsing.combinator.RegexParsers
import java.util.regex.Pattern

import scala.util.parsing.combinator.syntactical._
import scala.util.matching.Regex._
import Ptypes._
import Stypes._
import Etypes._
import SParser.StmtParser
import EParser.ExprParser
import TParser.TypeParser

object ProgramParser extends StandardTokenParsers with StmtParser
    with TypeParser with ExprParser{
//    lexical.delimiters ++= List("{","}")

    lexical.delimiters ++= List(";",":","=",",",".","(",")","+","-", "*", "/", "{", "}", "(", ")", "%", "[", "]")
    lexical.delimiters ++= List("++","--",">","<","==",">=","<=","!=", "!", "&&", "||")
    lexical.reserved.add("true")
    lexical.reserved.add("false")
    lexical.reserved.add("string")
    lexical.reserved.add("int")
    lexical.reserved.add("boolean")
    lexical.reserved.add("void")
    lexical.reserved.add("return")
    lexical.reserved.add("new")
    lexical.reserved.add("while")
    lexical.reserved.add("for")
    lexical.reserved.add("if")
    lexical.reserved.add("else")
    lexical.reserved.add("class")
    lexical.reserved.add("null")

    def typ = getTypeParser()
    def expr = getExprParser()
    def stmt = getStmtParser()
    def block : Parser[SBlock]= "{" ~ rep(stmt) ~ "}" ^^ { case _ ~ slist ~ _ => SBlock(slist) }

//    val escape: Parser[String] = regex("""\\.?""".r)
    def program = rep(classDef | fnDef) 
    def fnDef = typ ~ ident ~ "(" ~ repsep(arg,",") ~ ")" ~ block ^^ { case t ~ i ~ _ ~ params ~ _ ~ b => PFnDef(t, i, params, b) }
//    def typeDef = typ ~ ident ~ "(" ~ repsep(arg,",") ~ ")" ~ block ^^ { case t ~ i ~ _ ~ params ~ _ ~ b => PFnDef(t, i, params, b) }
//    def classDef = "class" ~ ident ~ "{" ~ "int" ~ ident ~ "}" ^^ {case _ ~ i1 ~ _ ~ _ ~ i2 ~ _ => Temp(i1, i2) }
    def classDef = "class" ~ ident ~ "{" ~ repsep((fnDef | decl), ";") ~ ";" ~ "}" ^^ { case _ ~ i ~ _ ~ fields ~ _ ~ _ => PCDef(i, fields) }
    def funItem = ident ^^ {i => Ident(i)}
    def arg = typ ~ funItem ^^ { case t ~ i => PArg(t,i) } 
    def decl = getTypeParser() ~ repsep(noInit, ",") ^^ { case t ~ items => PDecl(t, items) }


    def parse(s:String) = {
      val token = new lexical.Scanner(s)
      phrase(program)(token)
    }

    def test(s:String) = {
      parse(s) match {
        case Success(tree, _) =>
        case e: NoSuccess => 
          Console.err.println("ERROR\n")
          Console.err.println(e)
      }
    }

//    def main(args:Array[String]) = test(args(0))
}
