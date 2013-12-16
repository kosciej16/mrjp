package Stypes

import Ctypes._
import Etypes._
import Types._

sealed abstract class Stmt {}
sealed abstract class Item {}


case class IInit(ident:CIdent, expr:Expr) extends Item {}
case class INoInit(ident:CIdent) extends Item {}

case class SDecl(typ:Type, ident:List[Item]) extends Stmt {}
case class SAss(ident:CIdent, expr:Expr) extends Stmt {}
case class SInc(ident:CIdent) extends Stmt {}
case class SDecr(ident:CIdent) extends Stmt {}
case class SRet(expr:Expr) extends Stmt {}
case class SVRet() extends Stmt {}
case class SExpr(expr:Expr) extends Stmt {}
case class SCond(expr:Expr, stmt:Stmt) extends Stmt {}
case class SCondElse(expr:Expr, stmt1:Stmt, stmt2:Stmt) extends Stmt {}
case class SWhile(expr:Expr, stmt:Stmt) extends Stmt {}
case class SBlock(stmt:List[Stmt]) extends Stmt {}
