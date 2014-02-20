package Types

sealed abstract class Type {
    def getType() : String
    def getName() : String
}

case class TType(typ:String) extends Type {
  def getType() : String = typ
  def getName() : String = typ
}

case class TArray(typ:Type) extends Type {
  def getType() : String = "Array of " + typ.getType()
  def getName() : String = typ.getName()
}

case class TStruct(typ:String) extends Type {
  def getType() : String = "Struct " + typ
  def getName() : String = typ
}

case class TFun(typ:Type, params:List[Type]) extends Type {
  def getType() : String = typ.getType()
  def getName() : String = typ.getName()
}

