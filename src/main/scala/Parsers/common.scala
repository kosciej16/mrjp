package Ctypes

import Etypes._

trait Common extends Expr {}

case class CIdent(s:String) extends Common {
   def eval():Int = 0
 }
