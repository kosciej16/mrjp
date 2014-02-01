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
  var stringCount : Int = 0
  var endLabelCount : Int = 0
  val variables = scala.collection.mutable.Map[(CIdent,Int), Int]()
  val strings = scala.collection.mutable.Map[String, Int]()
  val func = scala.collection.mutable.Map[CIdent,(Type,List[PArg])]()

  def init(file : String) : Unit = {
    func.put(CIdent("printInt"), (TType("void"), List(PArg(TType("int"),CIdent("a")))))
    func.put(CIdent("printString"), (TType("void"), List(PArg(TType("string"),CIdent("a")))))
    func.put(CIdent("readString"), (TType("String"), List()))
    assCode += ".file \"a\"\n.text\n.data\n.globl _start\n"
    assCode += "strplace0\n"
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
          stmt(s, blockNr, labelCount)
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
      case SBlock(s) =>
        return countVarsBlock(SBlock(s))
      case _ =>
        return 0
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

  def item(input : Item, blockNr : Int) = {
    input match {
      case INoInit(id) => initVar(id, blockNr)
      case IInit(id, e) => initVar(id, blockNr, e)
    }
  }

  def initVar(id : CIdent, blockNr : Int, e : Expr = EConst(1)) = {
    variables.put((id, blockNr), varCount)
    expr(e,blockNr)
    assCode += "movl %eax, " + (-4)*varCount + "(%ebp)\n"
    varCount += 1
  }

  def getAdrress(id : CIdent, blockNr : Int) : Int = {
    if (variables.contains((id, blockNr))) {
      return variables.get((id, blockNr)).get*(-4)
    }
    return getAdrress(id, blockNr - 1)
  }


  def stmt(input : Stmt, blockNr : Int, labelNr : Int) : Unit = {
    var adrress=0
    input match {
      case SDecl(typ,itemList) => 
        for(i <- itemList)
          item(i, blockNr)
      case SDecr(id) => 
        adrress = getAdrress(id, blockNr)
        assCode += "movl " + adrress + "(%ebp) %eax\n"
        assCode += "subl $1, %eax\n"
        assCode += "movl %eax, " + adrress + "(%ebp)\n"
      case SInc(id) => assCode += ""
        adrress = getAdrress(id, blockNr)
        assCode += "movl " + adrress + "(%ebp) %eax\n"
        assCode += "addl $1, %eax\n"
        assCode += "movl %eax, " + adrress + "(%ebp)\n"
      case SExpr(e) => expr(e,blockNr)
      case SAss(id, e) => {
        expr(e,blockNr); 
        adrress = getAdrress(id, blockNr)
        assCode += "movl %eax, " + adrress + "(%ebp)\n"
      }
      case SVRet() => assCode += "jmp _return_" + currentFunId + "\n"
      case SRet(e) => expr(e,blockNr); assCode += "jmp _return_" + currentFunId + "\n"
      //ret
      case SCond(e,s) => 
        expr(e,blockNr)
        assCode += "cmpl $0, %eax\n"
        assCode += "je .endIf" + labelNr + "\n"
        stmt(s, blockNr, labelNr+1)
        assCode += ".endIf" + labelNr + ":\n"

      case SCondElse(e,s1, s2) => 
        expr(e,blockNr)
        assCode += "cmpl $0, %eax\n"
        assCode += "je .else" + labelNr + "\n"
        stmt(s1, blockNr, labelNr+2)
        assCode += "jmp .endElse" + (labelNr + 1) + "\n"
        assCode += ".else" + labelNr + ":\n"
        stmt(s2, blockNr, labelNr+2)
        assCode += ".endElse" + (labelNr + 1) + ":\n"

      case SWhile(e, s) =>
        assCode += "while_" + labelNr + ":\n"
        expr(e,blockNr)
        assCode += "cmpl $0, %eax\n"
        assCode += "je end_" + labelNr + "\n"
        stmt(s, blockNr, labelNr+2)
        assCode += "jmp while_" + labelNr + "\n"
        assCode += ".end" + labelNr + ":\n"
      case SBlock(x) => block(SBlock(x), blockNr + 1)
    }
  }

  def basicOperation(e1 : Expr, e2 : Expr, blockNr : Int) : Unit = {
    expr(e1,blockNr)
    assCode += "pushl %eax\n"
    expr(e2,blockNr)
    assCode += "popl %ebx\n"
  }
  def compareOperation(text : String, e1 : Expr, e2 : Expr, blockNr : Int) : Unit = {
    basicOperation(e1, e2, blockNr)
    assCode += "cmpl %eax, %ebx\n"
    assCode += "movl $1, %eax\n"
    assCode += text + " T"+labelCount+"\n"
    assCode += "movl $0, %eax\n"
    assCode += "T"+labelCount+":\n"
    labelCount += 1
  }

  def expr(input : Expr, blockNr : Int) : Unit = {
    input match {
      case ELitTrue() => assCode += "movl $1, %eax\n"
      case ELitFalse() => assCode += "movl $0, %eax\n"
      case CIdent(s) => {
        println(blockNr)
        val adrress = getAdrress(CIdent(s), blockNr)
        assCode += "movl " + adrress + "(%ebp), %eax\n"
      }
      case EConst(v) => assCode += "movl $" + v + ", %eax\n"
      case EString(s) => {
        if (!strings.contains(s)) {
          val newString = ".Str"+stringCount+":\n.string \""+s+"\"\nstrplace"+(stringCount+1)
          assCode = assCode.replaceFirst("strplace"+stringCount, newString)
          strings.put(s, stringCount)
          stringCount += 1
        }
        assCode += "movl $(.Str" + strings.get(s).get + "), %eax\n"
      }
      case EApp(i, l) => 
        for (e <- l) { expr(e,blockNr); assCode += "pushl %eax\n"; }
        assCode += "call " + i.getName() + "\n"
        for (e <- l) { assCode += "popl %ebx\n"; }
//        assCode += getMethodName(i)
      case EAdd(e1, e2) =>
        basicOperation(e1, e2, blockNr)
        assCode += "addl %ebx, %eax\n"
      case ESub(e1, e2) =>
        basicOperation(e1, e2, blockNr)
        assCode += "subl %ebx, %eax\n"
      case EMul(e1, e2) =>
        basicOperation(e1, e2, blockNr)
        assCode += "mull %ebx\n"
      case EDiv(e1, e2) =>
        expr(e1,blockNr); expr(e2,blockNr)
        assCode += "idiv\n"
      case EGt(e1, e2) =>
        //ifeq label
        compareOperation("jg", e1, e2, blockNr)
      case ELt(e1, e2) =>
        compareOperation("jl", e1, e2, blockNr)
      case EGeq(e1, e2) =>
        compareOperation("jge", e1, e2, blockNr)
      case ELeq(e1, e2) =>
        compareOperation("jle", e1, e2, blockNr)
      case EEq(e1, e2) =>
        compareOperation("je", e1, e2, blockNr)
      case ENeq(e1, e2) =>
        compareOperation("jne", e1, e2, blockNr)
      case EAnd(e1, e2) =>
        expr(e1,blockNr); 
        assCode += "cmpl $0, %eax\n"
        assCode += "je end" + endLabelCount + "\n"
        expr(e2,blockNr)
        assCode += "end" + endLabelCount + ":\n"
        endLabelCount += 1
      case EOr(e1, e2) =>
        expr(e1,blockNr); 
        assCode += "cmpl $1, %eax\n"
        assCode += "je end" + endLabelCount + "\n"
        expr(e2,blockNr)
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

  def main(args:Array[String]) = {
    val code = removeComments(read(args(0)))
    val tokens = ProgramParser.parse(code)
    val dir_name = (args(0).substring(0, args(0).lastIndexOf("/")+1))
    val file_name = ((args(0).substring(args(0).lastIndexOf("/")+1)))
    val index = file_name.indexOf(".")
    if (index != -1) {
      val pref_name = file_name.substring(0,file_name.indexOf("."))
    init(pref_name)
    ProgramParser.test(code)
    program(tokens)
   finish()
   println(dir_name)
   println(pref_name)
   val p = new java.io.PrintWriter(new File(dir_name + "out/" + pref_name + ".s"))
    p.println(assCode)
  p.close()
    }
  }
}
