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

  var needReturn : Boolean = true
  var assCode : String = ""
  var currentType : Char = ' '
  var varCount : Int = 1
  var pushCount : Int = 1
  var currentFunId : String = ""
  var currentStructId : String = ""
  var returnType : String = ""
  var labelCount : Int = 0
  var stringCount : Int = 0
  var endLabelCount : Int = 0
  val inheritance = Map[String, String]()
  var variables = Map[(LeftVar,Int), String]()
  val structTypes = Map[String, Map[String, String]]()
  val structFunc = Map[String, Map[String,(String,List[PArg])]]()
  var extendMap = Map[String, String]()
  val strings = Map[String, Int]()
  val func = Map[LeftVar,(String,List[PArg])]()

  def init() : Unit = {
    func.put(Ident("printInt"), ("void", List(PArg(TType("int"),Ident("a")))))
    func.put(Ident("printString"), ("void", List(PArg(TType("string"),Ident("a")))))
    func.put(Ident("readString"), ("string", List()))
    func.put(Ident("readInt"), ("int", List()))
    var map = Map[String,String]()
    map.put("length", "int")
    structTypes.put("Array", map)
  }

  def read(path : String) : String = {
    val lines = scala.io.Source.fromFile(path).mkString
    return lines
  }

  def program(input : PParser.ProgramParser.ParseResult[List[Prog]] ) : Unit = {
    input match {
      case PParser.ProgramParser.Success(tree, _) =>
        tree.map(readDef)
        tree.map(fnDef)
    }
  }

  def readDef(input : Prog) : Unit = {
    input match {
      case PFnDef(typ, id, l, b) => 
        func.put(id, (typ.getType(), l))
      //TODO


      case PCDef(name, fields) => 
      currentStructId = name
        var tmp = assCode
        structVars(name, fields)
        for (field <- fields) {
          field match {
            case PFnDef(typ, id, l, b) => {
              returnType = typ.getType()

              var fun = Map[String,(String,List[PArg])]()
              if (structFunc.contains(currentStructId))
                fun = structFunc.get(currentStructId).get
              fun.put(id.getName(), (typ.getType(), l))
              structFunc.put(name, fun)
              for (param <- l) {
                println("NOWA KLASOWA FUNKCJA", id, param)
                addVar(param.asIdent(), param.getName(), 0) 
              }
              block(b)
            }
            case _ =>
          }
        }
      case PCDefExt(name, ext, fields) => {
        structTypes.put(name, structTypes.get(ext).get)
        readDef(PCDef(name, fields))
      }
      case _ =>

    }
    currentStructId = ""
  }

  def structVars(name : String, fields : List[Prog]) : Unit = {
    var vars = Map[String, Int]()
    var types = Map[String, String]()
    if (structTypes.contains(name)) {
      types  = structTypes.get(name).get
    }
    for (field <- fields) {
      field match {
        case PDecl(typ, items) => {
          for (item <- items) {
            types.put(item.getName(), typ.getName())
          }
        }
        case _ =>
      }
    }
    structTypes.put(name, types)
  }

  def block(input : SBlock, blockNr : Int = 0) : Boolean = {
    var ret = false
    input match {
      case SBlock(stmtList) =>
        for(s <- stmtList)
          ret = ret | stmt(s, blockNr)
    }
    return ret || returnType == "void"
  }

  def varExists(id : LeftVar, blockNr : Int) = {
    if (variables.contains((id, blockNr))) {
      throw new IllegalStateException(id.getName() + " variable exists");
    }
  }

  def goodType(id : LeftVar, blockNr : Int, typ : String) : String = {
    println("goodType", id, blockNr, typ)
    if (blockNr < 0) {
      if(!structTypes.contains(currentStructId) || !structTypes.get(currentStructId).get.contains(id.getName()) ) {
        throw new IllegalStateException(id.getName() + " no variable");
      }
      else {
        println("IN STRUCT", structTypes.get(currentStructId).get.get(id.getName()).get)
        return structTypes.get(currentStructId).get.get(id.getName()).get
      }
    }
    if (!variables.contains((id, blockNr))) {
      goodType(id, blockNr - 1, typ)
    }
    else if (typ != "any" && variables.get(id, blockNr).get != typ) {
      throw new IllegalStateException(id.getName() + " bad type expected " + typ + " got " + variables.get(id, blockNr).get);
    }
    else return variables.get(id,blockNr).get
  }

  def expTypeList(input : Expr, blockNr : Int, types : List[String]) {
    val typ : String = getType(input, blockNr)
    println("EXPTYPELIST", input, typ)
    if (!types.contains(typ)) {
      throw new IllegalStateException(" bad expr type");
    }
  }

  def instance(t1 : String, t2 : String) : Boolean = {
    //TODO add inheritance
    false
  }

  def checkTypes(t1 : String, t2 : String = "int") : Unit = {
    println("CHECK TYPES")
    
    if (t1 != t2 && !instance(t1, t2)) {
      throw new IllegalStateException("type1 " + t1 + " type2 " + t2 + " types dont match");
    }
  }


  def expType(input : Expr, blockNr : Int, typ : String = "int", expType : String = "int") : String = {
    println("exp type")
    checkTypes(typ, expType)
    if (getType(input, blockNr) != typ) {
      println("EXPTYP", input, getType(input, blockNr), typ)
      throw new IllegalStateException(typ + " bad expr type");
    }
    return typ
  }

  def addVar(id : LeftVar, typ : String, blockNr : Int) = {
    varExists(id, blockNr)
    variables.put((id, blockNr), typ)
  }

  def removeBlock(blockNr : Int) = {
    for (((k,b),v) <- variables) {
      if (b == blockNr) variables.remove((k,b))
    }    
  }

  def fnDef(input : Prog) = {
    val blockNr = 0
    input match {
      case PFnDef(typ, id, l, b) => 
        returnType = typ.getType()
        for (param <- l) {
          println("NOWA FUNKCJA", id, param.getName())
          addVar(param.asIdent(), param.getName(), blockNr)
        }
        if (!block(b, 0)) throw new IllegalStateException("function " + id.getName() + " need return");
        removeBlock(0)
      //TODO
      case PCDef(name, fields) => 
      case PCDefExt(name, ext, fields) => 
      case _ =>

    }
  }

  def item(typ : String, input : Item, blockNr : Int) = {
    input match {
      case INoInit(id) => addVar(id, typ, blockNr)
      case IInit(id, e) => 
        expType(e, blockNr, typ, typ)
        addVar(id, typ, blockNr)
      //TODO
      case INewInit(id, RTable(t, e)) =>
        checkTypes(typ, "Array " + t)
        expType(e, blockNr, "int")
        println("ADD VAR", id)
        addVar(id, typ, blockNr)
      case INewInit(id, RStruct(s)) =>
        checkTypes(typ, s)
        addVar(id, typ, blockNr)
    }
  }

  def initVar(id : LeftVar, blockNr : Int, e : Expr = EConst(1)) = {
  }

  def stmt(input : Stmt, blockNr : Int) : Boolean = {
    input match {
      case SDecl(typ,itemList) =>
        for(i <- itemList)
          item(typ.getType(), i, blockNr)
        false
      case SDecr(id) => goodType(id, blockNr, "int"); false
      case SInc(id) => goodType(id, blockNr, "int"); false
      case SExpr(e) => getType(e, blockNr); false
      case SAss(Table(id, e1), e2) => 
        var typ : String = goodType(Ident(id), blockNr, "any")
        expType(e1, blockNr, "int")
        expType(e2, blockNr, typ.split(" ").last, typ.split(" ").last)
        false
      case SAss(id, e) => 
        var typ : String = goodType(id, blockNr, "any")
        expType(e, blockNr, typ, typ)
        false
      case SNewAss(id, r) => goodType(id, blockNr, r.getName()); false
      case SVRet() => checkTypes("void", returnType); true
      case SRet(e) => 
        checkTypes(getType(e, blockNr), returnType); true
      case SCond(e,s) => 
        getType(e, blockNr + 1)
        var b = false
        if (!e.isEvaluated() || e.eval() != 0) {
          b = stmt(s, blockNr)
        }
        if (e.isEvaluated() && e.eval() != 0) {
          return b
        }
        return false
      case SCondElse(e,s1, s2) => 
        getType(e, blockNr)
        var b = false
        if (!e.isEvaluated() || e.eval() != 0)
          b = stmt(s1, blockNr + 1)
        else
          return stmt(s2, blockNr + 1)
        if (!e.isEvaluated() || e.eval() == 0)
          b = b & stmt(s2, blockNr + 1)
        return b
      case SWhile(e, s) =>
        getType(e, blockNr)
        return stmt(s, blockNr + 1)
      //TODO
      case SFor(t, i1, i2, s) => false
      case SBlock(x) => 
        var b = block(SBlock(x), blockNr + 1)
        removeBlock(blockNr + 1)
        return b
    }
  }

  def appArgs(id : LeftVar, params : List[Expr], blockNr : Int) : String = {
    func.get(id).get match {
      case (typ, typList) => 
        if (typList.size != params.size) {
          throw new IllegalStateException(id.getName() + " bad number of arguments");
        }
        for((t,e) <- (typList zip params)) {
          checkTypes(getType(e, blockNr), t.getName())
        }
        println("APP ARGS", id, typ)
        return typ
    }
  }

  def callFun(id : LeftVar, name : String, params : List[Expr], blockNr : Int) : String = {
    //println("CALL FUN")
    var struct = variables.get(id,blockNr).get
    if (!structFunc.contains(struct))
      throw new IllegalStateException(struct + " Struct doesnt exist");
    var func = structFunc.get(struct).get
    if (!func.contains(name))
      throw new IllegalStateException(struct + " hasnt function " + name);
    return func.get(name).get._1
  }

  def getFieldType(name : String, field : String, blockNr : Int) : String = {
    var typ = variables.get((Ident(name), blockNr)).get
    typ = typ.split(" ")(0)
    if (!structTypes.contains(typ))
      throw new IllegalStateException(typ + " no such struct ")
    var vars = structTypes.get(typ).get
    if (!vars.contains(field))
      throw new IllegalStateException(typ + " hasnt field " + field)
    return vars.get(field).get
  }

  def getType(input : Expr, blockNr : Int) : String = {
    println("GET TYPE", input)
    var typ : String = "any"
    input match {
      case ELitTrue() => "boolean"
      case ELitFalse() => "boolean"
      case Ident(s) => { goodType(Ident(s), blockNr, "any") }
      //TODO
      case Table(s, e) => { getType(e, blockNr) }
      case Struct(s, field) => { getFieldType(s, field.getName(), blockNr) }
      case StructApp(s, EApp(fun, l)) => callFun(Ident(s), fun.getName(), l, blockNr)
      case RTable(_, e) => { "Rarray" }
      case RStruct(s) => { s }
      case EConst(v) => "int"
      case EString(s) => "string"
      //TODO
      case EApp(i, l) => appArgs(i, l, blockNr)
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
        expType(e1, blockNr, "boolean", "boolean")
      //TODO
      case ECast(t) => t
      case EGt(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return "boolean"
        //ifeq label
      case ELt(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return "boolean"
      case EGeq(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return "boolean"
      case ELeq(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return "boolean"
      case EEq(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return "boolean"
      case ENeq(e1, e2) =>
        typ = getType(e1, blockNr)
        expType(e2, blockNr, typ, typ)
        return "boolean"
      case EAnd(e1, e2) =>
        expTypeList(e1, blockNr, List("int", "boolean"))
        expTypeList(e2, blockNr, List("int", "boolean"))
        return "boolean"
      case EOr(e1, e2) => 
        expTypeList(e1, blockNr, List("int", "boolean"))
        expTypeList(e2, blockNr, List("int", "boolean"))
        return "boolean"
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

  def check(tokens : PParser.ProgramParser.ParseResult[List[Prog]]) = {
    init()
    program(tokens)
  }
}
