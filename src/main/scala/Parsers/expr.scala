package Etypes

trait Expr{
}

trait RightVar extends Expr {
  def getName() : String
}

case class RTable(s:String, expr:Expr) extends RightVar {
  def getName() : String = s
 }

case class RStruct(s:String) extends RightVar {
  def getName() : String = s
 }

trait LeftVar extends Expr {
  def getName() : String
}

case class Ident(s:String) extends LeftVar {
  def getName() : String = s
}

case class Table(s:String, e:Expr) extends LeftVar {
  def getName() : String = s
}

case class LeftItem(vals : List[LeftVar]) extends LeftVar {
  def getName() : String = ""
}

case class EApp(id:String, eList:List[Expr]) extends LeftVar {
  def getName() : String = ""
}

case class EConst(value:Int) extends Expr {
}

case class EString(value:String) extends Expr {
  }


case class EElem(ident:LeftVar, expr:Expr) extends Expr {}

case class ELitTrue() extends Expr {}

case class ELitFalse() extends Expr {}

case class EUMinus(e:Expr) extends Expr {}

case class EUNeg(e:Expr) extends Expr {}

case class ECast(id : String) extends Expr {}

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
