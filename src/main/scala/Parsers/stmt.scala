package Stypes

import Etypes._
import Types._

sealed abstract class Stmt {}
sealed abstract class Item {
    def getVar() : LeftVar
    def getName() : String 
}

case class IInit(ident:LeftVar, expr:Expr) extends Item {
    def getVar() : LeftVar = ident
    def getName() : String = ident.getName()
}
case class INewInit(ident:LeftVar, expr:RightVar) extends Item {
    def getVar() : LeftVar = ident
    def getName() : String = ident.getName()
}
case class INoInit(ident:LeftVar) extends Item {
    def getVar() : LeftVar = ident
    def getName() : String = ident.getName()
}

case class HTable(typ:String, expr:Expr) {}

case class SDecl(typ:Type, ident:List[Item]) extends Stmt {}
case class SAss(ident:LeftVar, expr:Expr) extends Stmt {}
case class SNewAss(ident:LeftVar, expr:RightVar) extends Stmt {}
case class SInc(ident:LeftVar) extends Stmt {}
case class SDecr(ident:LeftVar) extends Stmt {}
case class SRet(expr:Expr) extends Stmt {}
case class SVRet() extends Stmt {}
case class SExpr(expr:Expr) extends Stmt {}
case class SCond(expr:Expr, stmt:Stmt) extends Stmt {}
case class SCondElse(expr:Expr, stmt1:Stmt, stmt2:Stmt) extends Stmt {}
case class SWhile(expr:Expr, stmt:Stmt) extends Stmt {}
case class SFor(typ:Type, id1:String, id2:String, stmt:Stmt) extends Stmt {}
case class SBlock(stmt:List[Stmt]) extends Stmt {}
