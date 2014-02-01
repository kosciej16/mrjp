import java.io._
import Etypes._
import Types._
import Ctypes._
import Stypes._
import Ptypes._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._

import PParser.ProgramParser


object Assembly {

  var assCode : String = ""
  var currentType : Char = ' '
  var varCount : Int = 1
  var pushCount : Int = 1
  var currentFunId : String = ""
  var labelCount : Int = 0
  var endLabelCount : Int = 0
  val variables = scala.collection.mutable.Map[(CIdent,Int), Int]()
  val func = scala.collection.mutable.Map[CIdent,(Type,List[PArg])]()

  def init(file : String) : Unit = {
    func.put(CIdent("printInt"), (TType("void"), List(PArg(TType("int"),CIdent("a")))))
    func.put(CIdent("printString"), (TType("void"), List(PArg(TType("string"),CIdent("a")))))
    assCode += ".file \"a\"\n.text\n.data\n.globl _start\n"
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
          stmt(s, 0, labelCount)
    }
  }

  def fnDef(input : PFnDef) = {
    varCount = 1
    input match {
      case PFnDef(typ, id, l, b) => 
        currentFunId = id.getName()
        assCode += "_"+ id.getName() +":\npushl %ebp\nmovl %esp, %ebp\n"
        assCode += "subl $" + 4*countVarsBlock(b) + ", %ebp\n"
        block(b)
        assCode += "_return_" + currentFunId + ":\n"
        for (i <- 1 to pushCount) 
          assCode += "popl %ebp\n"
        assCode += "ret\n"
    }
  }

  def countVarsBlock(input : SBlock) : Int = {
    var variablesAmount = 0
    input match {
      case SBlock(stmtList) =>
        for(s <- stmtList)
          variablesAmount += countVars(s)
    }
    return variablesAmount
  }

  def countVars(input : Stmt) : Int = {
    input match {
      case SDecl(typ, itemList) =>
        return itemList.size
      case SCond(_, s) =>
        return countVars(s)
      case SCondElse(_, s1, s2) =>
        return math.max(countVars(s1), countVars(s2))
      case SWhile(e, s) =>
        return countVars(s)
      case _ =>
        return 0
      }
  }

  def item(input : Item, blockNr : Int) = {
    input match {
      case INoInit(id) => initVar(id, blockNr)
      case IInit(id, e) => initVar(id, blockNr, e)
    }
  }

  def initVar(id : CIdent, blockNr : Int, e : Expr = EConst(1)) = {
    variables.put((id, blockNr), varCount)
    expr(e)
    assCode += "movl %eax, " + (-4)*varCount + "(%ebp)\n"
    varCount += 1
  }

  def stmt(input : Stmt, blockNr : Int, labelNr : Int) : Unit = {
    var adrress=0
    input match {
      case SDecl(typ,itemList) => 
        for(i <- itemList)
          item(i, blockNr)
      case SDecr(id) => 
        adrress = variables.get((id, blockNr)).get*(-4)
        assCode += "movl " + adrress + "(%ebp) %eax\n"
        assCode += "subl $1 %eax\n"
        assCode += "movl %eax " + adrress + "(%ebp)\n"
      // movl -4(%ebp) - adrress %eax - register $1 < const
      // subl $1 %eax
      // movl %eax -4($ebp)
      // 
      case SInc(id) => assCode += ""
      //addl
      case SExpr(e) => expr(e)
      case SAss(id, e) => expr(e); assCode += ""
      case SVRet() => assCode += "jmp _return_" + currentFunId + "\n"
      case SRet(e) => expr(e); assCode += "jmp _return_" + currentFunId + "\n"
      //ret
      case SCond(e,s) => 
        expr(e)
        assCode += "cmpl $0, %eax\n"
        assCode += "je .endIf" + labelNr + "\n"
        stmt(s, blockNr, labelNr+1)
        assCode += ".endIf" + labelNr + ":\n"

      case SCondElse(e,s1, s2) => 
        expr(e)
        assCode += "cmpl $0, %eax\n"
        assCode += "je .else" + labelNr + "\n"
        stmt(s1, blockNr, labelNr+2)
        assCode += "jmp .endElse" + (labelNr + 1) + "\n"
        assCode += ".else" + labelNr + ":\n"
        stmt(s2, blockNr, labelNr+2)
        assCode += ".endElse" + (labelNr + 1) + ":\n"

      case SWhile(e, s) =>
        assCode += "while_" + labelNr + ":\n"
        expr(e)
        assCode += "cmpl $0, %eax\n"
        assCode += "je end_" + labelNr + "\n"
        stmt(s, blockNr, labelNr+2)
        assCode += "jmp while_" + labelNr + "\n"
        assCode += ".end" + labelNr + ":\n"
      case SBlock(x) => block(SBlock(x), blockNr + 1)
    }
  }

  def basicOperation(e1 : Expr, e2 : Expr) : Unit = {
    expr(e1)
    assCode += "pushl %eax\n"
    expr(e2)
    assCode += "popl %ebx\n"
  }
  def compareOperation(text : String, e1 : Expr, e2 : Expr) : Unit = {
    basicOperation(e1, e2)
    assCode += "cmpl %eax, %ebx\n"
    assCode += "movl $1, %eax\n"
    assCode += text + " T"+labelCount+"\n"
    assCode += "movl $0, %eax\n"
    assCode += "T"+labelCount+":\n"
    labelCount += 1
  }

  def expr(input : Expr) : Unit = {
    input match {
      case ELitTrue() => assCode += "movl $1, %eax\n"
      case ELitFalse() => assCode += "movl $0, %eax\n"
      case CIdent(s) => assCode += ""//"iload " + variables.get(CIdent(s)).get + "\n"
      case EConst(v) => assCode += "movl $" + v + ", %eax\n"
      case EString(s) => assCode += "ldc " + s + "\n"
      case EApp(i, l) => 
        for (e <- l) { expr(e); assCode += "pushl %eax\n"; }
        assCode += "call " + i.getName() + "\n"
        for (e <- l) { assCode += "popl %ebx\n"; }
//        assCode += getMethodName(i)
      case EAdd(e1, e2) =>
        basicOperation(e1, e2)
        assCode += "addl %ebx, %eax\n"
      case ESub(e1, e2) =>
        basicOperation(e1, e2)
        assCode += "subl %ebx, %eax\n"
      case EMul(e1, e2) =>
        basicOperation(e1, e2)
        assCode += "mull %ebx\n"
      case EDiv(e1, e2) =>
        expr(e1); expr(e2)
        assCode += "idiv\n"
      case EGt(e1, e2) =>
        //ifeq label
        compareOperation("jg", e1, e2)
      case ELt(e1, e2) =>
        compareOperation("jl", e1, e2)
      case EGeq(e1, e2) =>
        compareOperation("jge", e1, e2)
      case ELeq(e1, e2) =>
        compareOperation("jle", e1, e2)
      case EEq(e1, e2) =>
        compareOperation("je", e1, e2)
      case ENeq(e1, e2) =>
        compareOperation("jne", e1, e2)
      case EAnd(e1, e2) =>
        expr(e1); 
        assCode += "cmpl $0, %eax\n"
        assCode += "je end" + endLabelCount + "\n"
        expr(e2)
        assCode += "end" + endLabelCount + ":\n"
        endLabelCount += 1
      case EOr(e1, e2) =>
        expr(e1); 
        assCode += "cmpl $1, %eax\n"
        assCode += "je end" + endLabelCount + "\n"
        expr(e2)
        assCode += "end" + endLabelCount + ":\n"
        endLabelCount += 1
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

  def main(args:Array[String]) = {
    val code = removeComments(read(args(0)))
    println(code)
    val tokens = ProgramParser.parse(read(args(0)))
  /*  val dir_name = (args(0).substring(0, args(0).lastIndexOf("/")+1))
    val file_name = ((args(0).substring(args(0).lastIndexOf("/")+1)))
    val index = file_name.indexOf(".")
    if (index != -1) {
      val pref_name = file_name.substring(0,file_name.indexOf("."))
    init(pref_name)
    ProgramParser.test(read(args(0)))
  program(tokens)
    finish()
   println(dir_name)
   println(pref_name)
   val p = new java.io.PrintWriter(new File(dir_name + "out/" + pref_name + ".s"))
    p.println(assCode)
  p.close()
    }
    */
  }
}
