package Etypes

trait Expr{
  def eval():Int
}

case class EConst(value:Int) extends Expr {
  def eval():Int = value
}

case class EAdd(left:Expr, right:Expr) extends Expr {
    def eval():Int = left.eval() + right.eval()
}

case class ESub(left:Expr, right:Expr) extends Expr {
    def eval():Int = left.eval() - right.eval()
}

case class EMul(left:Expr, right:Expr) extends Expr {
    def eval():Int = left.eval() * right.eval()
}

case class EMod(left:Expr, right:Expr) extends Expr {
    def eval():Int = left.eval() % right.eval()
}

case class EDiv(left:Expr, right:Expr) extends Expr {
    def eval():Int = left.eval() / right.eval()
}

case class EGt(left:Expr, right:Expr) extends Expr {
  def eval():Int =
    if (left.eval() > right.eval) 1
    else 0
  }

case class ELt(left:Expr, right:Expr) extends Expr {
  def eval():Int =
    if (left.eval() < right.eval) 1
    else 0
  }

case class EGeq(left:Expr, right:Expr) extends Expr {
  def eval():Int =
    if (left.eval() >= right.eval) 1
    else 0
  }

case class ELeq(left:Expr, right:Expr) extends Expr {
  def eval():Int =
    if (left.eval() <= right.eval) 1
    else 0
  }

case class EEq(left:Expr, right:Expr) extends Expr {
  def eval():Int =
    if (left.eval() == right.eval) 1
    else 0
  }

case class ENeq(left:Expr, right:Expr) extends Expr {
  def eval():Int =
    if (left.eval() != right.eval) 1
    else 0
  }

case class ESum(sum:List[Expr]) extends Expr {
  def eval():Int = sum.foldLeft(0)(_+_.eval)
}
