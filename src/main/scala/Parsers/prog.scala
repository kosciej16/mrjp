package Ptypes

import Etypes._
import Types._
import Stypes._
import Ctypes._

sealed abstract class Prog {}

case class PFnDef(typ:Type, ident:CIdent, params:List[PArg], block:SBlock) extends Prog {}
case class PArg(typ:Type, ident:CIdent) extends Prog {}
