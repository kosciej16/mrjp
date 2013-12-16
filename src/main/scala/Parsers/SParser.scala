package SParser

import scala.util.parsing.combinator.syntactical._
import Stypes._
import EParser.ExprParser
import TParser.TypeParser
import CParser.CommonParser

trait StmtParser extends StandardTokenParsers with TypeParser
    with ExprParser with CommonParser{
  //    lexical.delimiters ++= List("{","}")

  def getStmtParser() : Parser[Stmt] = {


    def typ = getTypeParser()
    def expr = getExprParser()
//    def item : Parser[SIdent] = ident ^^ { s => SIdent(s) }

    def noInit : Parser[Item] = item ^^ { i => INoInit(i) }
    def init : Parser[Item] = item ~ "=" ~ expr ^^ { case i ~ _ ~ e => IInit(i, e) }
    def decl = typ ~ repsep(init | noInit, ",") ~ ";" ^^ { case t ~ items ~ _ => SDecl(t, items) }
    def ass = item ~ "=" ~ expr ~ ";" ^^ { case i ~ _ ~ e ~ _ => SAss(i, e) }
    def inc = item ~ "++" ~ ";" ^^ { case i ~ _ ~ _ => SInc(i) }
    def decr = item ~ "--" ~ ";" ^^ { case i ~ _ ~ _ => SDecr(i) }
    def ret = "return" ~ expr ~ ";" ^^ { case _ ~ e ~ _ => SRet(e) }
    def vret = "return" ~ ";" ^^ { x => SVRet() }
    def sexpr = expr ~ ";" ^^ { case e ~ _ => SExpr(e) }
    def cond : Parser[Stmt] = "if" ~ "(" ~ expr ~ ")" ~ stmt ^^ { case _ ~ _ ~ e ~ _ ~ s => SCond(e,s) }
    def condElse : Parser[Stmt] = "if" ~ "(" ~ expr ~ ")" ~ stmt ~ "else" ~ stmt ^^ 
      { case _ ~ _ ~ e ~ _ ~ s1 ~ _ ~ s2 => SCondElse(e,s1,s2) }
    def whileloop : Parser[Stmt] = "while" ~ "(" ~ expr ~ ")" ~ stmt ^^ { case _ ~ _ ~ e ~ _ ~ s => SWhile(e,s) }
    def block : Parser[SBlock]= "{" ~ rep(stmt) ~ "}" ^^ { case _ ~ slist ~ _ => SBlock(slist) }
 //   def block : Parser[Stmt]= "{" ~ rep(stmt) ~ "}" ^^ { case _ ~ slist ~ _ => SBlock(slist) }
    def stmt =  block | decl | ass | inc | decr | ret | vret | condElse | cond | whileloop | sexpr
    return stmt
  }
}

