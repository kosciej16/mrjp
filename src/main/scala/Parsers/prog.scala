package Ptypes

import Etypes._
import Types._
import Stypes._

sealed abstract class Prog {}

case class PFnDef(typ:Type, ident:LeftVar, params:List[PArg], block:SBlock) extends Prog {}
case class PCDef(name:String, fields:List[Prog]) extends Prog {}
case class PCDefExt(name:String, ext:String, fields:List[Prog]) extends Prog {}
case class PDecl(typ:Type, ident:List[INoInit]) extends Prog {}
case class Temp(s:String, s1:String) extends Prog {}
case class PArg(typ:Type, ident:LeftVar) extends Prog {
    def getType() : Type = typ
    def asIdent() : LeftVar = ident
}
