import java.io._
import Etypes._
import Types._
import Ctypes._
import Stypes._
import Ptypes._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._

import PParser.ProgramParser


object JVM {

  var jvmCode : String = ""
  var currentType : Char = ' '
  var varCount : Int = 0
  var labelCount : Int = 0
  val variables = scala.collection.mutable.Map[CIdent,Int]()
  val func = scala.collection.mutable.Map[CIdent,(Type,List[PArg])]()

  def init(file : String) : Unit = {
    func.put(CIdent("printInt"), (TType("void"), List(PArg(TType("int"),CIdent("a")))))
    func.put(CIdent("printString"), (TType("void"), List(PArg(TType("string"),CIdent("a")))))
    jvmCode += ".class public " + file + "\n" +
    ".super java/lang/Object\n"
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

  def fnDef(input : PFnDef) = {
    varCount = 0
    input match {
      case PFnDef(typ, id, l, b) => 
        jvmCode += ".method public static " + getMethodName(id, typ, l)
        jvmCode += ".limit locals 10\n.limit stack 10\n"
        block(b)
        jvmCode += ".end method\n"
    }
  }

  def block(input : SBlock) : Unit = {
    input match {
      case SBlock(stmtList) =>
        for(s <- stmtList)
          stmt(s, labelCount)
    }
  }

  def stmt(input : Stmt, labelNr : Int) : Unit = {
    input match {
      case SDecl(typ,itemList) => 
        currentType = typ.getType().charAt(0)
        itemList.map(item)
      case SDecr(id) => jvmCode += "iinc " + variables.get(id).get + " -1\n"
      case SInc(id) => jvmCode += "iinc " + variables.get(id).get + " 1\n"
      case SExpr(e) => expr(e)
      case SAss(id, e) => expr(e); jvmCode += "istore " + variables.get(id).get + "\n"
      case SVRet() => jvmCode += "return\n"
      case SRet(e) => expr(e); jvmCode += "ireturn\n"
      case SCond(e,s) => 
        //can it look worse?
        var tmp = labelNr
        tmp -= labelCount
        expr(e)
        tmp += labelCount
        jvmCode += "ifeq end_" + tmp + "\n"
        labelCount += 1
        stmt(s, labelCount)
        jvmCode += "end_" + tmp +":\n"

      case SCondElse(e,s1, s2) => 
        var tmp = labelNr
        tmp -= labelCount
        expr(e)
        tmp += labelCount
        jvmCode += "ifeq else_" + tmp + "\n"
        labelCount += 1
        stmt(s1, labelCount)
        jvmCode += "goto end_" + tmp + "\n"
        jvmCode += "else_" + tmp + ":\n"
        stmt(s2, labelCount)
        jvmCode += "end_" + tmp +":\n"

      case SWhile(e, s) =>
        jvmCode += "while_" + labelNr + ":\n"
        var tmp = labelNr
        tmp -= labelCount
        expr(e)
        tmp += labelCount
        jvmCode += "ifeq end_" + tmp + "\n"
        labelCount += 1
        stmt(s, labelCount)
        jvmCode += "goto while_" + labelNr + "\n"
        jvmCode += "end_" + tmp +":\n"
      case SBlock(x) => block(SBlock(x))
    }
  }

  def item(input : Item) = {
    input match {
      case INoInit(id) => initVar(id)
      case IInit(id, e) => initVar(id, e)
    }
  }

  def initVar(id : CIdent, e : Expr = EConst(0)) = {
    variables.put(id, varCount)
    expr(e)
    jvmCode += "istore " + varCount + "\n"
    varCount += 1
  }

  def fun : (Int,Int) => Int = (x,y) => 2*x

  def expr(input : Expr) : Unit = {
    input match {
      case ELitTrue() => jvmCode += "iconst_1"
      case ELitFalse() => jvmCode += "iconst_0 "
      case CIdent(s) => jvmCode += "iload " + variables.get(CIdent(s)).get + "\n"
      case EConst(v) => jvmCode += "ldc " + v + "\n"
      case EString(s) => jvmCode += "ldc " + s + "\n"
      case EApp(i, l) => 
        for (e <- l) expr(e)
        jvmCode += "invokestatic "
        if (i.getName() == "printInt" || i.getName() == "printString")
          jvmCode += "Runtime/"
        else 
          jvmCode += "Core/"
       jvmCode += getMethodName(i)
      case EAdd(e1, e2) =>
        expr(e1); expr(e2)
        jvmCode += "iadd\n"
      case ESub(e1, e2) =>
        expr(e1); expr(e2)
        jvmCode += "isub\n"
      case EMul(e1, e2) =>
        expr(e1); expr(e2)
        jvmCode += "imul\n"
      case EDiv(e1, e2) =>
        expr(e1); expr(e2)
        jvmCode += "idiv\n"
      case EGt(e1, e2) =>
        expr(e1); expr(e2)
        exprHelper("if_icmpgt")
      case ELt(e1, e2) =>
        expr(e1); expr(e2)
        exprHelper("if_icmplt")
      case EGeq(e1, e2) =>
        expr(e1); expr(e2)
        exprHelper("if_icmpge")
      case ELeq(e1, e2) =>
        expr(e1); expr(e2)
        exprHelper("if_icmple")
      case EEq(e1, e2) =>
        expr(e1); expr(e2)
        exprHelper("if_icmpeq")
      case EAnd(e1, e2) =>
        expr(e1)
        exprHelper("if_icmple")
      case EOr(e1, e2) =>
        expr(e1); expr(e2)
        exprHelper("if_icmple")
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


  def exprHelper(text : String) = {
    jvmCode += text + " true_"+labelCount+"\niconst_0\ngoto end_" + labelCount +
        "\ntrue_"+labelCount+":\niconst_1\nend_" + labelCount + ":\n"
    labelCount += 1
  }

  def finish() = {
    jvmCode +=".method public static main([Ljava/lang/String;)V\n" +
    ".limit locals 10\n.limit stack 10\n" +
    "invokestatic Core/main()I\npop\nreturn\n" + 
    ".end method"
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }


/*  def main(args:Array[String]) = {
    val tokens = ProgramParser.parse(read(args(0)))
    val dir_name = (args(0).substring(0, args(0).lastIndexOf("/")+1))
    val file_name = ((args(0).substring(args(0).lastIndexOf("/")+1)))
    val pref_name = file_name.substring(0,file_name.indexOf("."))
    init(pref_name)
    ProgramParser.test(read(args(0)))
    program(tokens)
    finish()
    println(dir_name)
    println(pref_name)
    val p = new java.io.PrintWriter(new File(dir_name + "out/" + pref_name + ".j"))
    p.println(jvmCode)
    p.close()
  } */
}
