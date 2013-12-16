package Types

sealed abstract class Type {
    def getType() : String
}

case class TType(typ:String) extends Type {
  def getType() : String = typ
}

case class TFun(typ:Type, params:List[Type]) extends Type {
  def getType() : String = typ.getType()
}

