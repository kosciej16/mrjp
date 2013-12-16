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

  def read() : String = {
    val lines = scala.io.Source.fromFile("/home/john/file.txt").mkString
    return lines
  }

  def program(input : PParser.ProgramParser.ParseResult[List[PFnDef]] ) : Unit = {
    input match {
      case PParser.ProgramParser.Success(tree, _) =>
        tree.map(fnDef)
    }
  }

  def fnDef(input : PFnDef) = {
    varCount = 0
    input match {
      case PFnDef(typ, id, l, b) => 
        block(b)
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
        var tmp = labelNr
        tmp -= labelCount
        expr(e)
        tmp += labelCount
        jvmCode += "if_cmpeq end_" + tmp + "\n"
        labelCount += 1
        stmt(s, labelCount)
        jvmCode += "end_" + tmp +":\n"
      case SCondElse(e,s1, s2) => 
        var tmp = labelNr
        tmp -= labelCount
        expr(e)
        tmp += labelCount
        jvmCode += "if_cmpeq else_" + tmp + "\n"
        labelCount += 1
        stmt(s1, labelCount)
        jvmCode += "goto end_" + tmp + "\n"
        jvmCode += "else_" + tmp + ":\n"
        stmt(s2, labelCount)
        jvmCode += "end_" + tmp +":\n"
      case SBlock(x) => block(SBlock(x))
    }
  }

  def item(input : Item) = {
    input match {
      case INoInit(id) => init(id)
      case IInit(id, e) => init(id, e)
    }
  }

  def init(id : CIdent, e : Expr = EConst(0)) = {
    variables.put(id, varCount)
    expr(e)
    jvmCode += "iload " + varCount + "\n"
    varCount += 1
  }

  def expr(input : Expr) : Unit = {
    var func : String = ""
    input match {
      case CIdent(s) => jvmCode += "iload " + variables.get(CIdent(s)).get + "\n"
      case EConst(v) => jvmCode += "ldc " + v + "\n"
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
        exprHelper("if_icmpgeq")
      }
  }

  def exprHelper(text : String) = {
    jvmCode += text + " false_"+labelCount+"\nconst_0\ngoto end_" + labelCount +
        "\nfalse_"+labelCount+":\nconst_1\nend_" + labelCount + ":\n"
    labelCount += 1
  }
  
  def main(args:Array[String]) = {
    val tokens = ProgramParser.parse(read())
    program(tokens)
    ProgramParser.test(read())
    println(jvmCode)
  }
}
