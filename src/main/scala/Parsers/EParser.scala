package EParser

import scala.util.parsing.combinator.syntactical._
import Etypes._


trait ExprParser extends StandardTokenParsers{

  def getExprParser () : Parser[Expr] = {


//    def unary : Parser[Expr] = "-" ~> (app | value) ^^ { EUMinus(_) } |
  //      "!" ~> (app | value) ^^ { EUNeg(_) }

    def parens : Parser[Expr] = "(" ~> expr <~ ")"

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
      "!" ~> (app | value) ^^ { e => EUNeg(e) } | cast | app | value | parens

//    def elem = item ~ "[" ~ expr ~ "]" ~ ";" ^^ { case i ~ _ ~ e ~ _ ~ _ => EElem(i, e) }

    def value = "true" ^^ { _ => ELitTrue() } |"false" ^^ { _ => ELitFalse() } | 
      numericLit ^^ { s => EConst(s.toInt) } | item |
      stringLit ^^ { s => EString(s)}

//TODO check it
    def cast = "(" ~ ident ~ ")" ~ "null" ^^ { case _ ~ i ~ _ ~ _ => ECast(i) }


    def expr = ( or | parens | value | item)          //top level expression
    return expr
  }

  def app : Parser[EApp] = item ~ "(" ~ repsep(getExprParser(), ",") ~ ")" ^^ { case i ~ _ ~ l ~ _ => EApp(i,l) }

  def item : Parser[LeftVar] = ident ~ "[" ~ getExprParser() ~ "]" ^^ { case i ~ _ ~ e ~ _ => Table(i,e) } |
    ident ~ "." ~ item ^^ { case i ~ _ ~ f => Struct(i,f) } | 
    ident ~ "." ~ app ^^ { case i ~ _ ~ a => StructApp(i,a) } | 
    ident ^^ { s => Ident(s) }

  def rightItem : Parser[RightVar] = 
    "new" ~ ("int" | "string" | "boolean") ~ "[" ~ getExprParser() ~"]" ^^ { case _ ~ t ~ _ ~ e ~ _ => RTable(t, e) } | 
    "new" ~ ident ^^ { case _ ~ s => RStruct(s) }
}
