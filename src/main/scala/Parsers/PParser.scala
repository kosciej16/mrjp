package PParser

import util.parsing.combinator.RegexParsers
import java.util.regex.Pattern

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.util.matching.Regex._
import Ptypes._
import Stypes._
import SParser.StmtParser
import EParser.ExprParser
import TParser.TypeParser

object ProgramParser extends StandardTokenParsers with StmtParser
    with TypeParser with ExprParser{
//    lexical.delimiters ++= List("{","}")

    lexical.delimiters ++= List(";",":","=",",",".","(",")","+","-", "*", "/", "{", "}", "(", ")", "%", "[", "]", " ")
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
    lexical.reserved.add("extends")
    lexical.reserved.add("null")

    def typ = getTypeParser()
    def expr = getExprParser()
    def stmt = getStmtParser()
    def block : Parser[SBlock]= "{" ~ rep(stmt) ~ "}" ^^ { case _ ~ slist ~ _ => SBlock(slist) }

    def program = rep(classDef | fnDef) 
    def fnDef = typ ~ item ~ "(" ~ repsep(arg,",") ~ ")" ~ block ^^ { case t ~ i ~ _ ~ params ~ _ ~ b => PFnDef(t, i, params, b) }
    def typeDef = typ ~ item ~ "(" ~ repsep(arg,",") ~ ")" ~ block ^^ { case t ~ i ~ _ ~ params ~ _ ~ b => PFnDef(t, i, params, b) }
//    def classDef = "class" ~ ident ~ "{" ~ "int" ~ ident ~ "}" ^^ {case _ ~ i1 ~ _ ~ _ ~ i2 ~ _ => Temp(i1, i2) }
    def classDef = "class" ~ ident ~ "{" ~ rep(fnDef | decl) ~ "}" ^^ { case _ ~ i ~ _ ~ fields ~ _ => PCDef(i, fields) } |
        "class" ~ ident ~ "extends" ~ ident ~ "{" ~ rep(fnDef | decl) ~ "}" ^^ { case _ ~ i ~ _ ~ i2 ~ _ ~ fields ~ _ => PCDefExt(i, i2, fields) }
    def arg = typ ~ item ^^ { case t ~ i => PArg(t,i) } 
    def decl = getTypeParser() ~ repsep(noInit, ",") ~ ";" ^^ { case t ~ items ~ _ => PDecl(t, items) }


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
