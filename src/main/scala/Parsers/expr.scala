package Etypes

trait Expr{
  def isEvaluated() : Boolean
  def eval() : Int
}

trait RightVar extends Expr {
  def getName() : String
}

case class RTable(s:String, expr:Expr) extends RightVar {
  def getName() : String = s
  def isEvaluated() : Boolean = false
  def eval() : Int = 0
 }

case class RStruct(s:String) extends RightVar {
  def getName() : String = s
  def isEvaluated() : Boolean = false
  def eval() : Int = 0
 }

trait LeftVar extends Expr {
  def getName() : String
}

case class Ident(s:String) extends LeftVar {
  def getName() : String = s
  def isEvaluated() : Boolean = false
  def eval() : Int = 0
 }

case class Table(s:String, expr:Expr) extends LeftVar {
  def getName() : String = s
  def isEvaluated() : Boolean = false
  def eval() : Int = 0
 }

case class Struct(s:String, field:LeftVar) extends LeftVar {
  def getName() : String = s
  def getField : String = field.getName()
  def isEvaluated() : Boolean = false
  def eval() : Int = 0
 }

case class TableStruct(t:Table, field:LeftVar) extends LeftVar {
  def getName() : String = t.getName()
  def getField : String = field.getName()
  def isEvaluated() : Boolean = false
  def eval() : Int = 0
 }

case class StructApp(s:String, app:EApp) extends LeftVar {
  def getName() : String = s
  def isEvaluated() : Boolean = false
  def eval() : Int = 0
 }

case class EConst(value:Int) extends Expr {
  def isEvaluated() : Boolean = true
  def eval() : Int = value
}

case class EString(value:String) extends Expr {
  def isEvaluated() : Boolean = true
  def eval() : Int = -10
  }

case class EApp(id:LeftVar, eList:List[Expr]) extends Expr {
  def isEvaluated() : Boolean = false
  def eval() : Int = 0
}

case class ELitTrue() extends Expr {
  def isEvaluated() : Boolean = true
  def eval() : Int = 1
}

case class ELitFalse() extends Expr {
  def isEvaluated() : Boolean = true
  def eval() : Int = 0
}

case class EUMinus(e:Expr) extends Expr {
  def isEvaluated() : Boolean = e.isEvaluated()
  def eval() : Int = -e.eval()
}

case class EUNeg(e:Expr) extends Expr {
  def isEvaluated() : Boolean = e.isEvaluated()
  def eval() : Int = if(e.eval() == 0) 1 else 0
}

case class ECast(id : String) extends Expr {
  def isEvaluated() : Boolean = false
  def eval() : Int = 0
}

case class EAdd(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = left.eval() + right.eval()
}

case class ESub(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = left.eval() - right.eval()
}

case class EMul(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = left.eval() * right.eval()
}

case class EMod(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = left.eval() % right.eval()
}

case class EDiv(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = left.eval() / right.eval()
}

case class EGt(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = if(left.eval() > right.eval()) 1 else 0
}

case class ELt(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = if(left.eval() < right.eval()) 1 else 0
}

case class EGeq(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = if(left.eval() >= right.eval()) 1 else 0
}

case class ELeq(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = if(left.eval() <= right.eval()) 1 else 0
}

case class EEq(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = if(left.eval() == right.eval()) 1 else 0
}

case class ENeq(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = if(left.eval() != right.eval()) 1 else 0
}

case class EAnd(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = if(left.eval() != 0 && right.eval() != 0) 1 else 0
}

case class EOr(left:Expr, right:Expr) extends Expr {
  def isEvaluated() : Boolean = left.isEvaluated() && right.isEvaluated()
  def eval() : Int = if(left.eval() != 0 || right.eval() != 0) 1 else 0
}
