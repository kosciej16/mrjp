import java.io._
import Etypes._
import Types._
import Stypes._
import Ptypes._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.collection.mutable._

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
  var variables = Map[(LeftVar,Int), Type]()
  val strings = Map[String, Int]()
  val func = Map[LeftVar,(Type,List[PArg])]()

  def init() : Unit = {
    func.put(Ident("printInt"), (TType("void"), List(PArg(TType("int"),Ident("a")))))
    func.put(Ident("printString"), (TType("void"), List(PArg(TType("string"),Ident("a")))))
    func.put(Ident("readString"), (TType("string"), List()))
    func.put(Ident("readInt"), (TType("int"), List()))
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

  def varExists(id : LeftVar, blockNr : Int) = {
    if (variables.contains((id, blockNr))) {
      throw new IllegalStateException(id.getName() + " variable exists");
    }
  }

  def goodType(id : LeftVar, blockNr : Int, typ : Type) : Type = {
    if (blockNr < 0) {
      throw new IllegalStateException(id.getName() + " no variable");
    }
    if (!variables.contains((id, blockNr))) {
      goodType(id, blockNr - 1, typ)
    }
    else if (typ.getType() != "any" && variables.get(id, blockNr).get != typ) {
      throw new IllegalStateException(id.getName() + " bad type");
    }
    else return variables.get(id,blockNr).get
  }

  def expTypeList(input : Expr, blockNr : Int, types : List[Type]) {
    val typ : Type = getType(input, blockNr)
    if (!types.contains(typ)) {
      throw new IllegalStateException(" bad expr type");
    }
  }

  def expType(input : Expr, blockNr : Int, typ : Type = TType("int"), expType : Type = TType("int")) : Type = {
    if (getType(input, blockNr) != typ || typ != expType) {
      throw new IllegalStateException(typ.getType() + " bad expr type");
    }
    return typ
  }

  def addVar(id : LeftVar, typ : Type, blockNr : Int) = {
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

  def initVar(id : LeftVar, blockNr : Int, e : Expr = EConst(1)) = {
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
      case SCond(e,s) => 
        getType(e, blockNr + 1)
        stmt(s, blockNr)
      case SCondElse(e,s1, s2) => 
        getType(e, blockNr)
        stmt(s1, blockNr + 1)
        stmt(s2, blockNr + 1)
      case SWhile(e, s) =>
        getType(e, blockNr)
        stmt(s, blockNr + 1)
      case SBlock(x) => 
        block(SBlock(x), blockNr + 1)
        removeBlock(blockNr + 1)
    }
  }

  def appArgs(id : LeftVar, params : List[Expr]) : Type = {
    func.get(id).get match {
      case (typ, typList) => return typ
    }
  }

  def getType(input : Expr, blockNr : Int) : Type = {
    var typ : Type = TType("any")
    input match {
      case ELitTrue() => TType("boolean")
      case ELitFalse() => TType("boolean")
      case Ident(s) => { goodType(Ident(s), blockNr, TType("any")) }
      case Table(s, e) => { TType("array")}
      case Struct(s, field) => { TType(s) }
      case RTable(_, e) => { TType("Rarray") }
      case RStruct(s) => { TType(s) }
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
      case EUNeg(e1) => 
        expType(e1, blockNr, TType("boolean"), TType("boolean"))
      case EGt(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return TType("boolean")
        //ifeq label
      case ELt(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return TType("boolean")
      case EGeq(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return TType("boolean")
      case ELeq(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return TType("boolean")
      case EEq(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return TType("boolean")
      case ENeq(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return TType("boolean")
      case EAnd(e1, e2) =>
        expTypeList(e1, blockNr, List(TType("int"), TType("boolean")))
        expTypeList(e2, blockNr, List(TType("int"), TType("boolean")))
        return TType("boolean")
      case EOr(e1, e2) => 
        expTypeList(e1, blockNr, List(TType("int"), TType("boolean")))
        expTypeList(e2, blockNr, List(TType("int"), TType("boolean")))
        return TType("boolean")
      }
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

  def check(tokens : PParser.ProgramParser.ParseResult[List[PFnDef]]) = {
    init()
    program(tokens)
  }
}
