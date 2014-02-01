package Etypes

import Ctypes._

trait Expr{
  def type():String
}

case class EConst(value:Int) extends Expr {
  def type():String = "Int"
}

case class EString(value:String) extends Expr {
    def eval():Int = "String"
  }

case class EApp(id:CIdent, eList:List[Expr]) extends Expr {}

case class ELitTrue() extends Expr {}

case class ELitFalse() extends Expr {}

case class EUMinus(e:Expr) extends Expr {}

case class EUNeg(e:Expr) extends Expr {}

case class EAdd(left:Expr, right:Expr) extends Expr {}

case class ESub(left:Expr, right:Expr) extends Expr {}

case class EMul(left:Expr, right:Expr) extends Expr {}

case class EMod(left:Expr, right:Expr) extends Expr {}

case class EDiv(left:Expr, right:Expr) extends Expr {}

case class EGt(left:Expr, right:Expr) extends Expr {}

case class ELt(left:Expr, right:Expr) extends Expr {}

case class EGeq(left:Expr, right:Expr) extends Expr {}

case class ELeq(left:Expr, right:Expr) extends Expr {}

case class EEq(left:Expr, right:Expr) extends Expr {}

case class ENeq(left:Expr, right:Expr) extends Expr {}

case class EAnd(left:Expr, right:Expr) extends Expr {}

case class EOr(left:Expr, right:Expr) extends Expr {}
