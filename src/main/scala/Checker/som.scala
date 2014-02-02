import java.io._
import Etypes._
import Types._
import Ctypes._
import Stypes._
import Ptypes._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._

import PParser.ProgramParser


object Checker {

  var assCode : String = ""
  var currentType : Char = ' '
  var varCount : Int = 1
  var pushCount : Int = 1
  var currentFunId : String = ""
  var labelCount : Int = 0
  var stringCount : Int = 0
  var endLabelCount : Int = 0
  var variables = scala.collection.mutable.Map[(CIdent,Int), Type]()
  val strings = scala.collection.mutable.Map[String, Int]()
  val func = scala.collection.mutable.Map[CIdent,(Type,List[PArg])]()

  def init() : Unit = {
    func.put(CIdent("printInt"), (TType("void"), List(PArg(TType("int"),CIdent("a")))))
    func.put(CIdent("printString"), (TType("void"), List(PArg(TType("string"),CIdent("a")))))
    func.put(CIdent("readString"), (TType("String"), List()))
  }

  def read(path : String) : String = {
    val lines = scala.io.Source.fromFile(path).mkString
    return lines
  }

  def program(input : PParser.ProgramParser.ParseResult[List[PFnDef]] ) : Unit = {
    input match {
      case PParser.ProgramParser.Success(tree, _) =>
        tree.map(read_fnDef)
        tree.map(fnDef)
    }
  }

  def read_fnDef(input : PFnDef) = {
    input match {
      case PFnDef(typ, id, l, b) => 
        func.put(id, (typ, l))
      }
  }

  def block(input : SBlock, blockNr : Int = 0) : Unit = {
    input match {
      case SBlock(stmtList) =>
        for(s <- stmtList)
          stmt(s, blockNr)
    }
  }

  def varExists(id : CIdent, blockNr : Int) = {
    println(id.getName(), blockNr)
    if (variables.contains((id, blockNr))) {
      throw new IllegalStateException(id.getName() + " variable exists");
    }
  }

  def goodType(id : CIdent, blockNr : Int, typ : Type) : Type = {
    if (!variables.contains((id, blockNr))) {
      throw new IllegalStateException(id.getName() + " no variable");
    }
    else if (typ.getType() != "any" && variables.get(id, blockNr).get != typ) {
      throw new IllegalStateException(id.getName() + " bad type");
    }
    return variables.get(id,blockNr).get
  }

  def expType(input : Expr, blockNr : Int, typ : Type = TType("int"), expType : Type = TType("int")) : Type = {
    if (getType(input, blockNr) != typ || typ != expType) {
      throw new IllegalStateException(typ.getType() + " bad expr type");
    }
    return typ
  }

  def addVar(id : CIdent, typ : Type, blockNr : Int) = {
    varExists(id, blockNr)
    variables.put((id, blockNr), typ)
  }

  def removeBlock(blockNr : Int) = {
    for (((k,b),v) <- variables) {
      if (b == blockNr) variables.remove((k,b))
    }    
  }

  def fnDef(input : PFnDef) = {
    val blockNr = 0
    input match {
      case PFnDef(typ, id, l, b) => 
        for (param <- l)
          addVar(param.asIdent(), param.getType(), blockNr)
          block(b, 0)
          removeBlock(0)
    }
  }

  def item(typ : Type, input : Item, blockNr : Int) = {
    input match {
      case INoInit(id) => variables.put((id, blockNr), typ)
      case IInit(id, e) => 
        expType(e, blockNr, typ, typ)
        variables.put((id, blockNr), typ)
    }
  }

  def initVar(id : CIdent, blockNr : Int, e : Expr = EConst(1)) = {
  }

  def stmt(input : Stmt, blockNr : Int) : Unit = {
    input match {
      case SDecl(typ,itemList) =>
        for(i <- itemList)
          item(typ, i, blockNr)
      case SDecr(id) => goodType(id, blockNr, TType("int"))
      case SInc(id) => goodType(id, blockNr, TType("int"))
      case SExpr(e) => getType(e, blockNr)
      case SAss(id, e) => 
        var typ : Type = goodType(id, blockNr, TType("any"))
        expType(e, blockNr, typ, typ)
      case SVRet() => assCode += "jmp _return_" + currentFunId + "\n"
      case SRet(e) => 
      //ret
      case SCond(e,s) => 

      case SCondElse(e,s1, s2) => 

      case SWhile(e, s) =>
      case SBlock(x) => 
        block(SBlock(x), blockNr + 1)
        removeBlock(blockNr + 1)
    }
  }

  def appArgs(id : CIdent, params : List[Expr]) : Type = {
    func.get(id).get match {
      case (typ, typList) => return typ
    }
  }

  def getType(input : Expr, blockNr : Int) : Type = {
    var typ : Type = TType("any")
    input match {
      case ELitTrue() => TType("bool")
      case ELitFalse() => TType("bool")
      case CIdent(s) => { variables.get((CIdent(s), blockNr)).get }
      case EConst(v) => TType("int")
      case EString(s) => TType("string")
      case EApp(i, l) => appArgs(i, l)
      case EAdd(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
      case ESub(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ)
      case EMul(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ)
      case EDiv(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ)
      case EMod(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ)
      case EUMinus(e1) => 
        expType(e1, blockNr)
      case EGt(e1, e2) => TType("bool")
        //ifeq label
      case ELt(e1, e2) => TType("bool")
      case EGeq(e1, e2) => TType("bool")
      case ELeq(e1, e2) => TType("bool")
      case EEq(e1, e2) => TType("bool")
      case ENeq(e1, e2) => TType("bool")
      case EAnd(e1, e2) => TType("bool")
      case EOr(e1, e2) =>  TType("bool")
      }
  }

  def convertType (typ : Type) : String = {
    println(typ.getType())
    typ.getType() match {
      case "string" => "Ljava/lang/String;"
      case "int" => "I"
      case "boolean" => "I"
      case "void" => "V"
    }
  }

  def getMethodName (id : CIdent) : String = {
    func.get(id).get match {
      case (typ, typList) =>
        getMethodName(id, typ, typList)
    }
  }

  def getMethodName (id : CIdent, typ : Type, typList : List[PArg]) : String = {
      id.getName()+"("+
      typList.foldLeft("")((acc, s) => convertType(s.getType()) + acc) +
      ")" + convertType(typ) +"\n"
  }

  def finish() = {
    assCode = assCode.replaceFirst("strplace"+stringCount+"\n","")
    assCode += "_start:\ncall _main\nmovl $1,%eax\nint  $0x80\n"
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def removeComments(code : String) : String = {
    return code.replaceAll("//.*\n", "\n").replaceAll("#.*\n", "\n").replaceAll("/\\*[\n|\\w\\W]*?\\*/", "\n")
  }

  //

  def check(tokens : PParser.ProgramParser.ParseResult[List[PFnDef]]) = {
    init()
    program(tokens)
  }
}
