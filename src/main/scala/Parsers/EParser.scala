package EParser

import scala.util.parsing.combinator.syntactical._
import Etypes._
import CParser.CommonParser


trait ExprParser extends StandardTokenParsers with CommonParser{

  def getExprParser () : Parser[Expr] = {


    def value = numericLit ^^ { s => EConst(s.toInt) } | item

    def oper = sum * (
      ">" ^^^ { (a:Expr, b:Expr) => EGt(a,b) } |
      "<" ^^^ { (a:Expr, b:Expr) => ELt(b,a) } |
      "==" ^^^ { (a:Expr, b:Expr) => EEq(a,b) } |
      "!=" ^^^ { (a:Expr, b:Expr) => ENeq(a,b) } |
      "<=" ^^^ { (a:Expr, b:Expr) => EGeq(a,b) } |
      ">=" ^^^ { (a:Expr, b:Expr) => ELeq(b,a) } )

    def product = value * (
      "*" ^^^ { (a:Expr, b:Expr) => EMul(a,b) } |
      "/" ^^^ { (a:Expr, b:Expr) => EDiv(a,b) } |
      "%" ^^^ { (a:Expr, b:Expr) => EMod(a,b) } )

    def sum = product * (
      "+" ^^^ { (a:Expr, b:Expr) => EAdd(a,b) } |
      "-" ^^^ { (a:Expr, b:Expr) => ESub(a,b) } )

    def expr = ( oper | value | item)          //top level expression
    return expr
  }

}
