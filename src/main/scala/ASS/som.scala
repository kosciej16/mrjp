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
  var currentFunId : String = ""
  var labelCount : Int = 0
  var stringCount : Int = 0
  var endLabelCount : Int = 0
  val variables = scala.collection.mutable.Map[(CIdent,Int), Int]()
  val checkerVariables = scala.collection.mutable.Map[(CIdent,Int), Type]()
  val strings = scala.collection.mutable.Map[String, Int]()
  val func = scala.collection.mutable.Map[CIdent,(Type,List[PArg])]()

  def init(file : String) : Unit = {
    func.put(CIdent("printInt"), (TType("void"), List(PArg(TType("int"),CIdent("a")))))
    func.put(CIdent("printString"), (TType("void"), List(PArg(TType("string"),CIdent("a")))))
    func.put(CIdent("readInt"), (TType("int"), List()))
    func.put(CIdent("readString"), (TType("string"), List()))
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
    removeBlock(blockNr)
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

  def addParams(params : List[PArg], blockNr : Int) = {
    varCount = 2
    for (param <- params) 
      param match {
        case PArg(typ, id) =>
          putVariable(id, blockNr, typ)
          varCount += 1
      }
      varCount = -1
  }

  def fnDef(input : PFnDef) = {
    varCount = 2
    input match {
      case PFnDef(typ, id, l, b) => 
        addParams(l.reverse, 0)
        currentFunId = id.getName()
        assCode += id.getName() +":\npushl %ebp\nmovl %esp, %ebp\n"
        assCode += "subl $" + 4*countVarsBlock(b) + ", %esp\n"
        block(b)
        assCode += "_return_" + currentFunId + ":\n"
        assCode += "addl $" + 4*countVarsBlock(b) + ", %esp\n"
        assCode += "popl %ebp\n"
        assCode += "ret\n"
    }
  }

  def item(input : Item, blockNr : Int, typ : Type) = {
    input match {
      case INoInit(id) => initVar(id, blockNr, typ, EConst(0))
      case IInit(id, e) => initVar(id, blockNr, typ, e)
    }
  }

  def noInit(id : CIdent, blockNr : Int, typ : Type) = {
    putVariable(id, blockNr, typ)
    varCount -= 1
  }

  def initVar(id : CIdent, blockNr : Int, typ : Type, e : Expr) = {
    expr(e,blockNr)
    putVariable(id, blockNr, typ)
    assCode += "movl %eax, " + 4*varCount + "(%ebp)\n"
    varCount -= 1
  }

  def getAdrress(id : CIdent, blockNr : Int) : Int = {
    if (variables.contains((id, blockNr))) {
      return variables.get((id, blockNr)).get*4
    }
    return getAdrress(id, blockNr - 1)
  }

  def removeBlock(blockNr : Int) = {
    for (((k,b),_) <- variables) {
      if (b == blockNr) variables.remove((k,b))
    }    
    for (((k,b),_) <- checkerVariables) {
      if (b == blockNr) checkerVariables.remove((k,b))
    }    
  }

  def stmt(input : Stmt, blockNr : Int, labelNr : Int) : Unit = {
    var adrress=0
    input match {
      case SDecl(typ,itemList) => 
        for(i <- itemList)
          item(i, blockNr, typ)
      case SDecr(id) => 
        adrress = getAdrress(id, blockNr)
        assCode += "movl " + adrress + "(%ebp), %eax\n"
        assCode += "subl $1, %eax\n"
        assCode += "movl %eax, " + adrress + "(%ebp)\n"
      case SInc(id) => assCode += ""
        adrress = getAdrress(id, blockNr)
        assCode += "movl " + adrress + "(%ebp), %eax\n"
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
        assCode += "je end" + labelNr + "\n"
        stmt(s, blockNr, labelNr+2)
        assCode += "jmp while_" + labelNr + "\n"
        assCode += "end" + labelNr + ":\n"
      case SBlock(x) =>
        labelCount = math.max(labelCount, labelNr)
        block(SBlock(x), blockNr + 1)
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
        
        Checker.variables = checkerVariables
        if (Checker.getType(e1, blockNr) == TType("string")) {
          assCode += "pushl %eax\n"
          assCode += "pushl %ebx\n"
          assCode += "call concat\n"
          assCode += "popl %ebx\n"
          assCode += "popl %ebx\n"
        }
        else {
          assCode += "addl %ebx, %eax\n"
        }

      case ESub(e1, e2) =>
        basicOperation(e2, e1, blockNr)
        assCode += "subl %ebx, %eax\n"
      case EMul(e1, e2) =>
        basicOperation(e1, e2, blockNr)
        assCode += "imull %ebx\n"
      case EDiv(e1, e2) =>
        basicOperation(e2,e1,blockNr)
        assCode += "movl $0, %edx\n"
        assCode += "idivl %ebx\n"
      case EMod(e1, e2) =>
      case EUMinus(e) => expr(e, blockNr); assCode += "negl %eax\n"
      case EUNeg(e) => {
        expr(e, blockNr)
        assCode += "cmpl $0, %eax\n"
        assCode += "sete %al\n"
        assCode += "movzbl %al, %eax\n"
      }

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
        assCode += "je false" + endLabelCount + "\n"
        expr(e2,blockNr)
        assCode += "cmpl $0, %eax\n"
        assCode += "je false" + endLabelCount + "\n"
        assCode += "movl $1, %eax\n"
        assCode += "jmp true" + endLabelCount + "\n"
        assCode += "false" + endLabelCount + ":\n"
        assCode += "movl $0, %eax\n"
        assCode += "true" + endLabelCount + ":\n"
        endLabelCount += 1
      case EOr(e1, e2) =>
        expr(e1,blockNr); 
        assCode += "cmpl $1, %eax\n"
        assCode += "je true" + endLabelCount + "\n"
        expr(e2,blockNr)
        assCode += "cmpl $1, %eax\n"
        assCode += "je true" + endLabelCount + "\n"
        assCode += "movl $0, %eax\n"
        assCode += "jmp false" + endLabelCount + "\n"
        assCode += "true" + endLabelCount + ":\n"
        assCode += "movl $1, %eax\n"
        assCode += "false" + endLabelCount + ":\n"
        endLabelCount += 1
      }
  }

  def putVariable(id : CIdent, blockNr : Int, typ : Type) = {
    variables.put((id, blockNr), varCount)
    checkerVariables.put((id, blockNr), typ)
    var i = 1;
    while (variables.contains((id, blockNr+i))) {
      variables.remove(id, blockNr+i)
      i = i+1;
    }
    i = 1;
    while (checkerVariables.contains((id, blockNr+i))) {
      checkerVariables.remove(id, blockNr+i)
      i = i+1;
    }
  }

  def finish() = {
    assCode = assCode.replaceFirst("strplace"+stringCount+"\n","")
    assCode += "_start:\ncall main\nmovl $1,%eax\nint  $0x80\n"
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
    val tokens = ProgramParser.parse(code)
    Checker.check(tokens)
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
   val p = new java.io.PrintWriter(new File(dir_name + pref_name + ".s"))
    p.println(assCode)
  p.close()
    } 
  }
}
