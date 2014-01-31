package EParser

import scala.util.parsing.combinator.syntactical._
import Etypes._
import CParser.CommonParser


trait ExprParser extends StandardTokenParsers with CommonParser{

  def getExprParser () : Parser[Expr] = {


//    def unary : Parser[Expr] = "-" ~> (app | value) ^^ { EUMinus(_) } |
  //      "!" ~> (app | value) ^^ { EUNeg(_) }


    def or = and * (
      "||" ^^^ { (a:Expr, b:Expr) => EOr(a,b) })

    def and = oper * (
      "&&" ^^^ { (a:Expr, b:Expr) => EAnd(a,b) })

    def oper = sum * (
      ">" ^^^ { (a:Expr, b:Expr) => EGt(a,b) } |
      "<" ^^^ { (a:Expr, b:Expr) => ELt(a,b) } |
      "==" ^^^ { (a:Expr, b:Expr) => EEq(a,b) } |
      "!=" ^^^ { (a:Expr, b:Expr) => ENeq(a,b) } |
      "<=" ^^^ { (a:Expr, b:Expr) => ELeq(a,b) } |
      ">=" ^^^ { (a:Expr, b:Expr) => EGeq(a,b) } )

    def sum = product * (
      "+" ^^^ { (a:Expr, b:Expr) => EAdd(a,b) } |
      "-" ^^^ { (a:Expr, b:Expr) => ESub(a,b) } )

    def product = unary * (
      "*" ^^^ { (a:Expr, b:Expr) => EMul(a,b) } |
      "/" ^^^ { (a:Expr, b:Expr) => EDiv(a,b) } |
      "%" ^^^ { (a:Expr, b:Expr) => EMod(a,b) } )

    def unary : Parser[Expr] = "-" ~> (app | value) ^^ { e => EUMinus(e) } | 
      "!" ~> (app | value) ^^ { e => EUNeg(e) } | app | value

    def value = "true" ^^ { _ => ELitTrue() } |"false" ^^ { _ => ELitFalse() } | 
      numericLit ^^ { s => EConst(s.toInt) } | item |
      stringLit ^^ { s => EString(s)}

    def app : Parser[Expr] = item ~ "(" ~ repsep(expr, ",") ~ ")" ^^ { case i ~ _ ~ l ~ _ => EApp(i,l) }


    def expr = ( or | value | item)          //top level expression
    return expr
  }

}
