//TODO
// a = a.next a.next.elem

import sys.process._
import java.io._
import Etypes._
import Types._
import Stypes._
import Ptypes._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.collection.mutable._

import PParser.ProgramParser


object Assembly {

  var assCode : String = ""
  var currentType : Char = ' '
  var varCount : Int = 1
  var currentFunId : String = ""
  var currentStructId : String = ""
  var labelCount : Int = 0
  var stringCount : Int = 0
  var endLabelCount : Int = 0
  var secretVariable : LeftVar = Ident("secretVariable")
  val varTypes = Map[String, String]()
  val structs = Map[String, Map[String, Int]]()
  val structTypes = Map[String, Map[String, String]]()
  val inheritance = Map[String, String]()
  val variables = Map[(LeftVar,Int), Int]()
  val checkerVariables = Map[(LeftVar,Int), Type]()
  val strings = Map[String, Int]()
  val func = Map[LeftVar,(Type,List[PArg])]()

  def init(file : String) : Unit = {
    func.put(Ident("printInt"), (TType("void"), List(PArg(TType("int"),Ident("a")))))
    func.put(Ident("printString"), (TType("void"), List(PArg(TType("string"),Ident("a")))))
    func.put(Ident("readInt"), (TType("int"), List()))
    func.put(Ident("readString"), (TType("string"), List()))
    var map = Map[String,Int]()
    map.put("length", -1)
    structs.put("Array", map)
    assCode += ".file \"a\"\n.text\n.data\n.globl main\n"
    assCode += "strplace0\n"
    addString("%d\\\\n")
    addString("%s\\\\n")
    // assCode += ".LC0:\n\t.string \"%d\"\n"
  }

  def read(path : String) : String = {
    val lines = scala.io.Source.fromFile(path).mkString
    return lines
  }

  def program(input : PParser.ProgramParser.ParseResult[List[Prog]] ) : Unit = {
    input match {
      case PParser.ProgramParser.Success(tree, _) =>
        tree.map(read_fnDef)
        tree.map(progDef)
      case _ =>
    }
  }

  def read_fnDef(input : Prog) = {
    input match {
      case PFnDef(typ, id, l, b) => 
        func.put(Ident(id), (typ, l))
      case _ => 
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
      case SWhile(_, s) =>
        return countVars(s)
      case SFor(_, _, _, s) =>
        return countVars(s) + 2
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
        case PArg(TArray(typ), id) => {
          putVariable(id, blockNr, typ)
          varCount += 1
          varTypes.put(id.getName(), "Array")
        }
        case PArg(TStruct(typ), id) => {
          varTypes.put(id.getName(), typ)
          putVariable(id, blockNr, TType(typ))
          varCount += 1
        }
        case PArg(typ, id) => {
          putVariable(id, blockNr, typ)
          varCount += 1
        }
      }
      varCount = -1
  }

  def progDef(input : Prog) = {
    varCount = 2
    input match {
      case PFnDef(typ, id, l, b) => 
        addParams(l.reverse, 0)
        currentFunId = id
        assCode += id +":\npushq %rbp\nmovq %rsp, %rbp\n"
        assCode += "subq $" + 8*countVarsBlock(b) + ", %rsp\n"
        block(b)
        assCode += "_return_" + currentFunId + ":\n"
        // assCode += "addq $" + 4*countVarsBlock(b) + ", %rsp\n"
        assCode += "leave\n"
        assCode += "ret\n"
      case PCDef(name, fields) => {
        // println("PCDef")
        structVars(name, fields)
        currentStructId = name
        for (field <- fields) {
          field match {
            case PFnDef(typ, id, l, b) => {
              // println("PFnDef")
              l :+ (PArg(TStruct(name), Ident("this")))
              addParams(l.reverse, 0)
              currentFunId = id
              assCode += currentStructId + "_" + id +":\npushq %rbp\nmovq %rsp, %rbp\n"
              assCode += "subq $" + 4*countVarsBlock(b) + ", %rsp\n"
              block(b)
              assCode += "_return_" + currentStructId + "_" + currentFunId + ":\n"
              // assCode += "addq $" + 4*countVarsBlock(b) + ", %rsp\n"
              assCode += "leave\n"
              assCode += "ret\n"
            }
            case _ =>
          }
        }
      }
      currentStructId = ""
      currentFunId = ""
      //TODO ?
      case _ =>
    }
  }

  def structVars(name : String, fields : List[Prog]) : Unit = {
    varCount = 0
    var vars = Map[String, Int]()
    var types = Map[String, String]()
    for (field <- fields) {
      field match {
        case PDecl(typ, items) => {
          // println("PDecl")
          for (item <- items) {
            // println(item)
            vars.put(item.getName(), varCount)
            types.put(item.getName(), typ.getName())
            varCount += 1
          }
        }
        case _ =>
      }
    }
    // println("struct put", name)
    structs.put(name, vars)
    structTypes.put(name, types)
  }

  def item(input : Item, blockNr : Int, labelNr : Int) = {
    input match {
      case IInit(id, e) => stmt(SAss(id, e), blockNr, labelNr)
      case INewInit(id, r) => {
        stmt(SNewAss(id, r), blockNr, labelNr)
      }
      case INoInit(id) => stmt(SAss(id, EConst(0)), blockNr, labelNr)
    }
  }

  def createArray(id : LeftVar, blockNr : Int, e : Expr, typ : String = "Array") = {
    expr(e, blockNr)
    callMalloc()
    val adrress = getAdrress(id, blockNr)
    assCode += "movq %rax, " + adrress + "(%rbp)\n"
    varTypes.put(id.getName(), typ)
  }

  def callMalloc() = {
    assCode += "pushq %rax\n"
    assCode += "call allocate_array\n"
    assCode += "popq %rbx\n"
  }



  def putArray(id : LeftVar) = {
    var map = Map[String,Int]()
    if (structs.contains(id.getName())) {
      map = structs.get(id.getName()).get
    }
    map.put("length", -1)
    structs.put(id.getName(), map)
  }

  def initVar(id : LeftVar, blockNr : Int, typ : Type, e : Expr) = {
    expr(e,blockNr)
    putVariable(id, blockNr, typ)
    assCode += "movq %rax, " + 4*varCount + "(%rbp)\n"
    varCount -= 1
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
    // println(input)
    input match {
      case SDecl(TArray(typ),itemList) => 
        for(id <- itemList) {
          putVariable(id.getVar(), blockNr, typ)
          varTypes.put(id.getName(), "Array")
          varCount -= 1
          item(id, blockNr, labelNr)
        }
      case SDecl(TStruct(typ),itemList) => 
        for(id <- itemList) {
          putVariable(id.getVar(), blockNr, TType(typ))
          varTypes.put(id.getName(), typ)
          varCount -= 1
          item(id, blockNr, labelNr)
        }
      case SDecl(typ,itemList) => 
        for(id <- itemList) {
          // putVariable(id.getVar(), blockNr, typ)
          putVariable(id.getVar(), blockNr, typ)
          varCount -= 1
          item(id, blockNr, labelNr)
          swapVariable(id.getVar(), blockNr, typ)
        }
      case SDecr(id) => 
        adrress = getAdrress(id, blockNr)
        assCode += "movq " + adrress + "(%rbp), %rax\n"
        assCode += "subq $1, %rax\n"
        assCode += "movq %rax, " + adrress + "(%rbp)\n"
      case SInc(id) => assCode += ""
        adrress = getAdrress(id, blockNr)
        assCode += "movq " + adrress + "(%rbp), %rax\n"
        assCode += "addq $1, %rax\n"
        assCode += "movq %rax, " + adrress + "(%rbp)\n"
      case SExpr(e) => expr(e,blockNr)
      case SAss(s, e) => {
        // println("LEFTITEM" + l)
        expr(e,blockNr); 
        adrress = getAdrress(s, blockNr)
        assCode += "movq %rax, " + adrress + "(%rbp)\n"
      }
    /*  case SAss(Table(id, index), e) => {
        adrress = getAdrress(Ident(id), blockNr)
        assCode += "movq " + adrress + "(%rbp), %rax\n"
        assCode += "pushq %rax\n" 
        expr(index,blockNr); 
        assCode += "pushq %rax\n" 
        expr(e,blockNr); 
        assCode += "pushq %rax\n" 
        assCode += "call setValue\n"
        for (i <- 1 to 3) assCode += "popq %rbx\n"
      }
      case SAss(Struct(id, field), e) => {
        val index = expandStruct(id, field, blockNr)
        stmt(SAss(Table(id, EConst(index._1)), e), blockNr, labelNr)
      } */
      case SNewAss(id, RTable(t, e)) => {
        createArray(id, blockNr, e)
      }
      case SNewAss(id, RStruct(s)) => {
        val size = getStructSize(id.getName())
        createArray(id, blockNr, EConst(size), s)
      }
      case SVRet() => assCode += "jmp _return_" + currentFunId + "\n"
      case SRet(e) => expr(e,blockNr); assCode += "jmp _return_" + currentFunId + "\n"
      // TODO STRUCT
      // case SVRet() => assCode += "jmp _return_" + currentStructId + "_" + currentFunId + "\n"
      // case SRet(e) => expr(e,blockNr); assCode += "jmp _return_" + currentStructId + "_" + currentFunId + "\n"
      //ret
      case SCond(e,s) => 
        expr(e,blockNr)
        assCode += "cmpq $0, %rax\n"
        assCode += "je .endIf" + labelNr + "\n"
        stmt(s, blockNr, labelNr+1)
        assCode += ".endIf" + labelNr + ":\n"

      case SCondElse(e,s1, s2) => 
        expr(e,blockNr)
        assCode += "cmpq $0, %rax\n"
        assCode += "je .else" + labelNr + "\n"
        stmt(s1, blockNr, labelNr+2)
        assCode += "jmp .endElse" + (labelNr + 1) + "\n"
        assCode += ".else" + labelNr + ":\n"
        stmt(s2, blockNr, labelNr+2)
        //TODO po co to?
        assCode += ".endElse" + (labelNr + 1) + ":\n"

      case SWhile(e, s) =>
        assCode += "while_" + labelNr + ":\n"
        expr(e,blockNr)
        assCode += "cmpq $0, %rax\n"
        assCode += "je end" + labelNr + "\n"
        stmt(s, blockNr, labelNr+2)
        assCode += "jmp while_" + labelNr + "\n"
        assCode += "end" + labelNr + ":\n"
      case SFor(t, i1, i2, s) => {
        labelCount += 1
        putVariable(Ident("myindex"), blockNr, t)
        varCount -= 1
        putVariable(Ident(i1), blockNr, t)
        varCount -= 1
        //TODO
        val indexAdrress = getAdrress(Ident("myindex"), blockNr)
        val adrress = getAdrress(Ident(i1), blockNr)
        val arrayAdrress = getAdrress(Ident(i2), blockNr)
        assCode += "movq $0, " + indexAdrress + "(%rbp)\n"
        assCode += "movq " + arrayAdrress + "(%rbp), %rax\n"
        assCode += "pushq %rax\n"
        assCode += "movq $-1, %rax\n"
        assCode += "pushq %rax\n"
        assCode += "call getValue\n"
        assCode += "movq %rax, %ecx\n"
        assCode += "popq %rbx\n"
        assCode += "popq %rbx\n"
        assCode += "for" + labelNr + ":\n"
        assCode += "movq " +indexAdrress + "(%rbp), %rax\n"
        assCode += "cmpq %ecx, %rax\n"
        assCode += "jge end" + labelNr + "\n"
        assCode += "pushq %ecx\n"
        assCode += "movq " + arrayAdrress + "(%rbp), %rax\n"
        assCode += "pushq %rax\n"
        assCode += "movq " + indexAdrress + "(%rbp), %rax\n"
        assCode += "pushq %rax\n"
        assCode += "call getValue\n"
        assCode += "popq %rbx\n"
        assCode += "popq %rbx\n"
        assCode += "movq %rax, " + adrress + "(%rbp)\n"
        stmt(s, blockNr, labelNr+1)
        assCode += "movq " + indexAdrress + "(%rbp), %rax\n"
        assCode += "addq $1, %rax\n"
        assCode += "movq %rax, " + indexAdrress + "(%rbp)\n"
        assCode += "popq %ecx\n"
        assCode += "jmp for" + labelNr + "\n"
        assCode += "end" + labelNr + ":\n"
      }
        
      case SBlock(x) =>
        labelCount = math.max(labelCount, labelNr)
        block(SBlock(x), blockNr + 1)
      // TODO
      case _ => 
    }
  }

  def basicOperation(e1 : Expr, e2 : Expr, blockNr : Int) : Unit = {
    expr(e1,blockNr)
    assCode += "pushq %rax\n"
    expr(e2,blockNr)
    assCode += "popq %rbx\n"
  }
  def compareOperation(text : String, e1 : Expr, e2 : Expr, blockNr : Int) : Unit = {
    basicOperation(e1, e2, blockNr)
    assCode += "cmpq %rax, %rbx\n"
    assCode += "movq $1, %rax\n"
    assCode += text + " T"+labelCount+"\n"
    assCode += "movq $0, %rax\n"
    assCode += "T"+labelCount+":\n"
    labelCount += 1
  }

  def expandStruct(id : String, field : LeftVar, blockNr : Int) : (Int, String) = {
    // println ("expand", id, field)
    var typ = ""
    if (varTypes.contains(id)) {
      typ = varTypes.get(id).get
    }
    else {
      typ = id
    }
    //TODO more dots
    // println("type", typ)
    val index = structs.get(typ).get.get(field.getName()).get
    // println("got index", index)
 //   expr(Table(id, EConst(index)), blockNr)
    val newId = structTypes.get(typ).get.get(field.getName()).get
    // println("match time")
    field match {
  //    case Struct(_, f) => expandStruct(newId, f, blockNr)
      //TODO APP can return reference to struct
    //  case StructApp(s, EApp(i, l) => expr(EApp(Struct //sxpandStruct(newId, f, blockNr)
//      case Table(i, e) => expr(Table(i, EConst(index)), blockNr);
      case _ =>
    }
    return (index,"")
   // println( structs.get(typ).get.get(field.getName()).get)

  }

  def getAdrress(idd : LeftVar, blockNr : Int) : Int = {
    val id = Ident(idd.getName())
    // println (id)
    if (variables.contains((id, blockNr))) {
      return variables.get((id, blockNr)).get*8
    }
    if (blockNr < 0) return -1;
    return getAdrress(id, blockNr - 1)
  }


  def getStructField(id : String, field : LeftVar, varName : Boolean = true) : Int = {
    // println ("struct", id)
    var typ : String = "" 
    if (varName) {
      typ = varTypes.get(id).get
    }
    else {
      typ = id
    }
    //TODO more dots
    // println ("struct2", typ, field)
    // println( structs.get(typ).get.get(field.getName()).get)
    return structs.get(typ).get.get(field.getName()).get
  }

  def getArrayValue(id : LeftVar, blockNr : Int, e : Expr) : Unit = {
  }

  def getStructSize(id : String) : Int = {
    // println("get size", id)
    val name = varTypes.get(id).get
    // println("size name", name)
    return structs.get(name).get.size
  }

  def addString(id : String) : Unit = {
    val newString = ".Str"+stringCount+":\n.string \""+id+"\"\nstrplace"+(stringCount+1)
    assCode = assCode.replaceFirst("strplace"+stringCount, newString)
    strings.put(id, stringCount)
    stringCount += 1
  }

  def expr(input : Expr, blockNr : Int) : Unit = {
    input match {
      case ELitTrue() => assCode += "movq $1, %rax\n"
      case ELitFalse() => assCode += "movq $0, %rax\n"
      case Ident(s) => {
        val adrress = getAdrress(Ident(s), blockNr)
        //TODO correct to check if currentStructId = ""
        if (adrress != -1) {
          assCode += "movq " + adrress + "(%rbp), %rax\n"
        }
        else {
          val index = getStructField(currentStructId, Ident(s), false)
          assCode += "movq 8(%rbp), %rax\n"
          assCode += "pushq %rax\n"
          assCode += "movq $" + index + ", %rax\n"
          assCode += "pushq %rax\n"
          assCode += "call getValue\n"
          for (i <- 1 to 2) assCode += "popq %rbx\n"
        }
      }
  /*    case Table(s, e) => {
        val adrress = getAdrress(Ident(s), blockNr)
        assCode += "movq " + adrress + "(%rbp), %rax\n"
        assCode += "pushq %rax\n"
        expr(e, blockNr)
        assCode += "pushq %rax\n"
        assCode += "call getValue\n"
        for (i <- 1 to 2) assCode += "popq %rbx\n"
        
      }
      case Struct(s, field) => {
        expandStruct(s, field, blockNr)
      }
      */
      case RTable(_, e) => {
        expr(e, blockNr)
      }
      case RStruct(s) => {
        val size = getStructSize(s)
      }
      case EConst(v) => assCode += "movq $" + v + ", %rax\n"
      case EString(s) => {
        if (!strings.contains(s)) {
          addString(s)
        }
        assCode += "movq $(.Str" + strings.get(s).get + "), %rax\n"
      }
  /*    case EApp(Struct(s, fun), l) => 
        val adrress = getAdrress(Ident(s), blockNr)
        assCode += "movq " + adrress + "(%rbp), %rax\n"
        assCode += "pushq %rax\n"
        for (e <- l) { expr(e,blockNr); assCode += "pushq %rax\n"; }
        assCode += "call " + varTypes.get(s).get + "_" + fun.getName() + "\n"
        assCode += "pop %rbx\n"
        for (e <- l) { assCode += "popq %rbx\n"; }
        */
        
      case EApp(i, l) => 
        // println(i)
        if (i == "printInt") {
          expr(l.head, blockNr)
          assCode += "movq %rax, %rsi\nmovq $.Str0, %rdi\nmovq $0, %rax\ncall printf\n"
        }
        else if (i == "printString") {
          expr(l.head, blockNr)
          assCode += "movq %rax, %rsi\nmovq $.Str1, %rdi\nmovq $0, %rax\ncall printf\n"
        }
        else {
          for (e <- l) { expr(e,blockNr); assCode += "pushq %rax\n"; }
          assCode += "call " + i + "\n"
          for (_ <- l) { assCode += "popq %rbx\n"; }
        }
        // for (e <- l) { assCode += "popq %rbx\n"; }
//        assCode += getMethodName(i)
      case LeftItem(l) =>
        expr(l.head, blockNr)
      case EAdd(e1, e2) =>
        basicOperation(e1, e2, blockNr)
        
        Checker.variables = checkerVariables
        if (Checker.getType(e1, blockNr) == TType("string")) {
          assCode += "movq %rax, %rsi\nmovq %rbx, %rdi\ncall strcat\n"
        }
        else {
          assCode += "addq %rbx, %rax\n"
        }

      case ESub(e1, e2) =>
        basicOperation(e2, e1, blockNr)
        assCode += "subq %rbx, %rax\n"
      case EMul(e1, e2) =>
        basicOperation(e1, e2, blockNr)
        assCode += "imulq %rbx\n"
      case EDiv(e1, e2) =>
        basicOperation(e2,e1,blockNr)
        assCode += "movq $0, %rdx\n"
        assCode += "idivq %rbx\n"
      case EMod(e1, e2) =>
        basicOperation(e2,e1,blockNr)
        assCode += "movq $0, %rdx\n"
        assCode += "idivq %rbx\n"
        assCode += "mov %rdx, %rax\n"
      case EUMinus(e) => expr(e, blockNr); assCode += "negq %rax\n"
      case EUNeg(e) => {
        expr(e, blockNr)
        assCode += "cmpq $0, %rax\n"
        assCode += "sete %al\n"
        assCode += "movzbq %al, %rax\n"
      }
      case ECast(_) => {
        assCode += "movq $0, %rax\n"
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
        assCode += "cmpq $0, %rax\n"
        assCode += "je false" + endLabelCount + "\n"
        expr(e2,blockNr)
        assCode += "cmpq $0, %rax\n"
        assCode += "je false" + endLabelCount + "\n"
        assCode += "movq $1, %rax\n"
        assCode += "jmp true" + endLabelCount + "\n"
        assCode += "false" + endLabelCount + ":\n"
        assCode += "movq $0, %rax\n"
        assCode += "true" + endLabelCount + ":\n"
        endLabelCount += 1
      case EOr(e1, e2) =>
        expr(e1,blockNr); 
        assCode += "cmpq $1, %rax\n"
        assCode += "je true" + endLabelCount + "\n"
        expr(e2,blockNr)
        assCode += "cmpq $1, %rax\n"
        assCode += "je true" + endLabelCount + "\n"
        assCode += "movq $0, %rax\n"
        assCode += "jmp false" + endLabelCount + "\n"
        assCode += "true" + endLabelCount + ":\n"
        assCode += "movq $1, %rax\n"
        assCode += "false" + endLabelCount + ":\n"
        endLabelCount += 1
      }
  }

  def putVariable(idd : LeftVar, blockNr : Int, typ : Type) = {
    // val id : LeftVar = Ident(idd.getName())
    val id : LeftVar = secretVariable
    // println ("put variable", id, blockNr)
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
    println(variables)
  }

  def swapVariable(id : LeftVar, blockNr : Int, typ : Type) = {
    println(id)
    println(blockNr)
    var varCount : Option[Int] = variables.get((secretVariable, blockNr))
    variables.remove(secretVariable, blockNr)
    variables.put((id, blockNr), varCount.get)
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
    println(variables)
  }

  def finish() = {
    assCode = assCode.replaceFirst("strplace"+stringCount+"\n","")
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
      // println(code)
    val tokens = ProgramParser.parse(code)
      // Checker.check(tokens)
    val dir_name = (args(0).substring(0, args(0).lastIndexOf("/")+1))
    val file_name = ((args(0).substring(args(0).lastIndexOf("/")+1)))
    val index = file_name.indexOf(".")
    if (index != -1) {
      val pref_name = dir_name + file_name.substring(0,file_name.indexOf("."))
      init(pref_name)
      ProgramParser.test(code)
      program(tokens)
      finish()
      // println(dir_name)
      // println(pref_name)
      val p = new java.io.PrintWriter(new File(pref_name + ".s"))
      for (line <- assCode.split('\n')) {
        if (line.last != ':')
          p.print('\t')
          p.println(line)
      }
      p.close()
      println("gcc -c " + pref_name + ".s")
      "gcc -c " + pref_name + ".s -o" + pref_name + ".o" !;
      "gcc -o " + pref_name + " " + pref_name + ".o" !;;
      System.err.println("OK\n")
    } 
  }
}
