package SParser

import scala.util.parsing.combinator.syntactical._
import Stypes._
import EParser.ExprParser
import TParser.TypeParser

trait StmtParser extends StandardTokenParsers with TypeParser
    with ExprParser{
  //    lexical.delimiters ++= List("{","}")

  def noInit : Parser[INoInit] = item ^^ { i => INoInit(i) }

  def getStmtParser() : Parser[Stmt] = {


    def typ = getTypeParser()
    def expr = getExprParser()
//    def item : Parser[SIdent] = ident ^^ { s => SIdent(s) }

    def decl = typ ~ repsep(init | noInit, ",") ~ ";" ^^ { case t ~ items ~ _ => SDecl(t, items) }
    def init : Parser[Item] = 
      item ~ "=" ~ rightItem ^^ { case i ~ _ ~ t => INewInit(i, t) } |
      item ~ "=" ~ getExprParser() ^^ { case i ~ _ ~ e => IInit(i, e) }
    def ass = item ~ "=" ~ rightItem ~ ";" ^^ { case i ~ _ ~ t ~ _ => SNewAss(i, t) } |
      item ~ "=" ~ expr ~ ";" ^^ { case i ~ _ ~ e ~ _ => SAss(i, e) } 
    def inc = item ~ "++" ~ ";" ^^ { case i ~ _ ~ _ => SInc(i) }
    def decr = item ~ "--" ~ ";" ^^ { case i ~ _ ~ _ => SDecr(i) }
    def ret = "return" ~ expr ~ ";" ^^ { case _ ~ e ~ _ => SRet(e) }
    def vret = "return" ~ ";" ^^ { x => SVRet() }
    def sexpr = expr ~ ";" ^^ { case e ~ _ => SExpr(e) }
    def cond : Parser[Stmt] = "if" ~ "(" ~ expr ~ ")" ~ stmt ^^ { case _ ~ _ ~ e ~ _ ~ s => SCond(e,s) }
    def condElse : Parser[Stmt] = "if" ~ "(" ~ expr ~ ")" ~ stmt ~ "else" ~ stmt ^^ 
      { case _ ~ _ ~ e ~ _ ~ s1 ~ _ ~ s2 => SCondElse(e,s1,s2) }
    def whileloop : Parser[Stmt] = "while" ~ "(" ~ expr ~ ")" ~ stmt ^^ { case _ ~ _ ~ e ~ _ ~ s => SWhile(e,s) }
    def forloop : Parser[Stmt] = "for" ~ "(" ~ typ ~ ident ~ ":" ~ ident ~ ")" ~ stmt ^^ { case _ ~ _ ~ t ~ i1 ~ _ ~ i2 ~ _ ~ s => SFor(t,i1,i2,s) }
    def block : Parser[SBlock]= "{" ~ rep(stmt) ~ "}" ^^ { case _ ~ slist ~ _ => SBlock(slist) }
    def stmt =  block | decl | ass | inc | decr | ret | vret | condElse | cond | whileloop | forloop | sexpr
    return stmt
  }
}

