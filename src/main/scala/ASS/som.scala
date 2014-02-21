//TODO
// a = a.next a.next.elem

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
  val varTypes = Map[String, String]()
  val classCodes = Map[String, String]()
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
    assCode += ".file \"a\"\n.text\n.data\n.globl _start\n"
    assCode += "strplace0\n"
  }

  def read(path : String) : String = {
    val lines = scala.io.Source.fromFile(path).mkString
    return lines
  }

  def program(input : PParser.ProgramParser.ParseResult[List[Prog]] ) : Unit = {
    input match {
      case PParser.ProgramParser.Success(tree, _) =>
        tree.map(readDef)
        tree.map(progDef)
    }
  }

  def readDef(input : Prog) : Unit = {
    input match {
      case PFnDef(typ, id, l, b) => 
        func.put(id, (typ, l))
      case PCDef(name, fields) => {
        //TODO check if order of declare is important
        var tmp = assCode
        assCode = ""
        println("PCDef")
        structVars(name, fields)
        currentStructId = name
        for (field <- fields) {
          field match {
            case PFnDef(typ, id, l, b) => {
              println("PFnDef")
              l :+ (PArg(TStruct(name), Ident("this")))
              addParams((l :+ (PArg(TStruct(name), Ident("this")))).reverse, 0)
              currentFunId = id.getName()
              assCode += currentStructId + id.getName() +":\npushl %ebp\nmovl %esp, %ebp\n"
              assCode += "subl $" + 4*countVarsBlock(b) + ", %esp\n"
              block(b)
              assCode += "_return_" + currentStructId + currentFunId + ":\n"
              assCode += "addl $" + 4*countVarsBlock(b) + ", %esp\n"
              assCode += "popl %ebp\n"
              assCode += "ret\n"
            }
            case _ =>
          }
        }
        classCodes.put(name, assCode)
        assCode = tmp + assCode
        currentStructId = ""
        currentFunId = ""
      }
      case PCDefExt(name, ext, fields) => {
        structs.put(name, structs.get(ext).get)
        structTypes.put(name, structTypes.get(ext).get)
        readDef(PCDef(name, fields))
        assCode += classCodes.get(ext).get.replaceAll(ext, name)
        println("KOD DLA KLASY", ext)
        println(classCodes.get(ext).get)
        println("ORAZ KOD DLA KLASY", name)
        println(classCodes.get(name).get)
        //println((classCodes.get(ext).get + classCodes.get(name).get).replaceAll(ext, name))
        classCodes.put(name, (classCodes.get(ext).get + classCodes.get(name).get).replaceAll(ext, name))
      }
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
        currentFunId = id.getName()
        assCode += id.getName() +":\npushl %ebp\nmovl %esp, %ebp\n"
        assCode += "subl $" + 4*countVarsBlock(b) + ", %esp\n"
        block(b)
        assCode += "_return_" + currentFunId + ":\n"
        assCode += "addl $" + 4*countVarsBlock(b) + ", %esp\n"
        assCode += "popl %ebp\n"
        assCode += "ret\n"
        case _ =>
    }
    currentFunId = ""
  }

  def structVars(name : String, fields : List[Prog]) : Unit = {
    varCount = 0
    var vars = Map[String, Int]()
    var types = Map[String, String]()
    if (structs.contains(name)) {
      vars = structs.get(name).get
      varCount = structs.get(name).size
      types  = structTypes.get(name).get
    }
    for (field <- fields) {
      field match {
        case PDecl(typ, items) => {
          println("PDecl")
          for (item <- items) {
            println(item)
            vars.put(item.getName(), varCount)
            types.put(item.getName(), typ.getName())
            varCount += 1
          }
        }
        case _ =>
      }
    }
    println("struct put", name)
    structs.put(name, vars)
    structTypes.put(name, types)
  }

  def item(input : Item, blockNr : Int, labelNr : Int) = {
    input match {
      case IInit(id, e) => stmt(SAss(id, e), blockNr, labelNr)
      case INewInit(id, r) => {
        stmt(SNewAss(id, r), blockNr, labelNr)
      }
      case _ =>
    }
  }

  def createArray(id : LeftVar, blockNr : Int, e : Expr, typ : String = "Array") = {
    id match {
      case Ident(s) =>
        expr(e, blockNr)
        callMalloc()
        val adrress = getAdrress(id, blockNr)
        assCode += "movl %eax, " + adrress + "(%ebp)\n"
        varTypes.put(s, typ)
      case Struct(s, f) =>
        expandStruct(s, f, blockNr)
        val index = getStructIndex(s, f)
        //adrress = getAdrress(Ident(id), blockNr)
        //assCode += "movl " + adrress + "(%ebp), %eax\n"
        assCode += "pushl %eax\n" 
        assCode += "movl $" + index + ", %eax\n"
        assCode += "pushl %eax\n" 
        expr(e,blockNr); 
        assCode += "pushl %eax\n" 
        assCode += "call setValue\n"
        for (i <- 1 to 3) assCode += "popl %ebx\n"
      }
  }

  def callMalloc() = {
    assCode += "pushl %eax\n"
    assCode += "call allocate_array\n"
    assCode += "popl %ebx\n"
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
    assCode += "movl %eax, " + 4*varCount + "(%ebp)\n"
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
    input match {
      case SDecl(TArray(typ),itemList) => 
      //TODO right side should be expr before add leftVar
        for(id <- itemList) {
          putVariable(id.getVar(), blockNr, typ)
          varTypes.put(id.getName(), "Array")
          varCount -= 1
          item(id, blockNr, labelNr)
        }
      case SDecl(TStruct(typ),itemList) => 
        for(id <- itemList) {
          println("decl struct", id.getName(), typ)
          putVariable(id.getVar(), blockNr, TType(typ))
          varTypes.put(id.getName(), typ)
          varCount -= 1
          item(id, blockNr, labelNr)
        }
      case SDecl(typ,itemList) => 
        for(id <- itemList) {
          id match {
            case IInit(i, e) => 
              expr(e, blockNr)
              putVariable(id.getVar(), blockNr, typ)
              varTypes.put(id.getName(), typ.getType())
              adrress = getAdrress(i, blockNr)
              assCode += "movl %eax, " + adrress + "(%ebp)\n"
              varCount -= 1
            case INoInit(i) =>
              putVariable(id.getVar(), blockNr, typ)
              varTypes.put(id.getName(), typ.getType())
              adrress = getAdrress(i, blockNr)
              assCode += "movl $0, " + adrress + "(%ebp)\n"
              varCount -= 1
          }
        }
      case SDecr(id) => 
        stmt(SAss(id, EAdd(id, EConst(-1))), blockNr, labelNr)
/*        adrress = getAdrress(id, blockNr)
        assCode += "movl " + adrress + "(%ebp), %eax\n"
        assCode += "subl $1, %eax\n"
        assCode += "movl %eax, " + adrress + "(%ebp)\n" */
      case SInc(id) => assCode += ""
        stmt(SAss(id, EAdd(id, EConst(1))), blockNr, labelNr)
/*        adrress = getAdrress(id, blockNr)
        assCode += "movl " + adrress + "(%ebp), %eax\n"
        assCode += "addl $1, %eax\n"
        assCode += "movl %eax, " + adrress + "(%ebp)\n" */
      case SExpr(e) => expr(e,blockNr)
      case SAss(Ident(s), e) => {
        adrress = getAdrress(Ident(s), blockNr)
        if (adrress != -1) {
          expr(e,blockNr); 
          assCode += "movl %eax, " + adrress + "(%ebp)\n"
        }
        else { setStructField(currentStructId, Ident(s), e, blockNr) }

      }
      case SAss(Table(id, index), e) => {
        adrress = getAdrress(Ident(id), blockNr)
        assCode += "movl " + adrress + "(%ebp), %eax\n"
        assCode += "pushl %eax\n" 
        expr(index,blockNr); 
        assCode += "pushl %eax\n" 
        expr(e,blockNr); 
        assCode += "pushl %eax\n" 
        assCode += "call setValue\n"
        for (i <- 1 to 3) assCode += "popl %ebx\n"
      }
      case SAss(Struct(id, field), e) => {
        //expandStruct(id, field, blockNr)
        adrress = getAdrress(Ident(id), blockNr)
        val index = getStructIndex(id, field)
        stmt(SAss(Table(id, EConst(index)), e), blockNr, labelNr)
      }
      case SNewAss(id, RTable(t, e)) => {
        createArray(id, blockNr, e)
      }
      case SNewAss(id, RStruct(s)) => {
        val size = getStructSize(id.getName())
        createArray(id, blockNr, EConst(size), s)
      }
      case SVRet() => assCode += "jmp _return_" + currentStructId + currentFunId + "\n"
      case SRet(e) => expr(e,blockNr); assCode += "jmp _return_" + currentStructId + currentFunId + "\n"
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
        assCode += "movl $0, " + indexAdrress + "(%ebp)\n"
        assCode += "movl " + arrayAdrress + "(%ebp), %eax\n"
        assCode += "pushl %eax\n"
        assCode += "movl $-1, %eax\n"
        assCode += "pushl %eax\n"
        assCode += "call getValue\n"
        assCode += "movl %eax, %ecx\n"
        assCode += "popl %ebx\n"
        assCode += "popl %ebx\n"
        assCode += "for" + labelNr + ":\n"
        assCode += "movl " +indexAdrress + "(%ebp), %eax\n"
        assCode += "cmpl %ecx, %eax\n"
        assCode += "jge end" + labelNr + "\n"
        assCode += "pushl %ecx\n"
        assCode += "movl " + arrayAdrress + "(%ebp), %eax\n"
        assCode += "pushl %eax\n"
        assCode += "movl " + indexAdrress + "(%ebp), %eax\n"
        assCode += "pushl %eax\n"
        assCode += "call getValue\n"
        assCode += "popl %ebx\n"
        assCode += "popl %ebx\n"
        assCode += "movl %eax, " + adrress + "(%ebp)\n"
        stmt(s, blockNr, labelNr+1)
        assCode += "movl " + indexAdrress + "(%ebp), %eax\n"
        assCode += "addl $1, %eax\n"
        assCode += "movl %eax, " + indexAdrress + "(%ebp)\n"
        assCode += "popl %ecx\n"
        assCode += "jmp for" + labelNr + "\n"
        assCode += "end" + labelNr + ":\n"
      }
        
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

  def expandStruct(id : String, field : LeftVar, blockNr : Int) : Unit = {
    println ("expand", id, field)
    var typ = ""
    if (varTypes.contains(id)) {
      typ = varTypes.get(id).get
    }
    else {
      typ = id
    }
    //TODO more dots
    val index = structs.get(typ).get.get(field.getName()).get
    expr(Table(id, EConst(index)), blockNr)
   // println( structs.get(typ).get.get(field.getName()).get)

  }

  def getAdrress(idd : LeftVar, blockNr : Int) : Int = {
    val id = Ident(idd.getName())
    println ("address", id, blockNr)
    if (variables.contains((id, blockNr))) {
      return variables.get((id, blockNr)).get*4
    }
    if (blockNr < 0) {
      return -1;
    }
    return getAdrress(id, blockNr - 1)
  }


  def getStructIndex(id : String, field : LeftVar, varName : Boolean = true) : Int = {
    println ("struct", id)
    var typ : String = "" 
    if (varName) {
      typ = varTypes.get(id).get
    }
    else {
      typ = id
    }
    //TODO more dots
    println ("struct2", typ, field)
    println( structs.get(typ).get.get(field.getName()).get)
    return structs.get(typ).get.get(field.getName()).get
  }

  def getArrayValue(id : LeftVar, blockNr : Int, e : Expr) : Unit = {
  }

  def getStructSize(id : String) : Int = {
    println("get size", id)
    val name = varTypes.get(id).get
    println("size name", name)
    return structs.get(name).get.size
  }

  def getStructField(str : String, s : LeftVar, varName : Boolean = true) : Unit = {
    val index = getStructIndex(str, s, varName)
    assCode += "movl 8(%ebp), %eax\n"
    assCode += "pushl %eax\n"
    assCode += "movl $" + index + ", %eax\n"
    assCode += "pushl %eax\n"
    assCode += "call getValue\n"
    for (i <- 1 to 2) assCode += "popl %ebx\n"
  }

  def setStructField(str : String, s : LeftVar, e : Expr, blockNr : Int) : Unit = {
    val index = getStructIndex(str, s, false)
    assCode += "movl 8(%ebp), %eax\n"
    assCode += "pushl %eax\n"
    assCode += "movl $" + index + ", %eax\n"
    assCode += "pushl %eax\n"
    println("expression set struct", e)
    expr(e, blockNr)
    assCode += "pushl %eax\n"
    assCode += "call setValue\n"
    for (i <- 1 to 3) assCode += "popl %ebx\n"
  }
  def expr(input : Expr, blockNr : Int) : Unit = {
    input match {
      case ELitTrue() => assCode += "movl $1, %eax\n"
      case ELitFalse() => assCode += "movl $0, %eax\n"
      case Ident(s) => {
        val adrress = getAdrress(Ident(s), blockNr)
        //TODO correct to check if currentStructId = ""
        if (adrress != -1) {
          assCode += "movl " + adrress + "(%ebp), %eax\n"
        }
        else {
          getStructField(currentStructId, Ident(s), false)
        }
      }
      case Table(s, e) => {
        if (currentStructId != "") {
          //setStructField
        }
        if (currentStructId == "") {
          val adrress = getAdrress(Ident(s), blockNr)
          assCode += "movl " + adrress + "(%ebp), %eax\n"
          assCode += "pushl %eax\n"
          expr(e, blockNr)
          assCode += "pushl %eax\n"
          assCode += "call getValue\n"
          for (i <- 1 to 2) assCode += "popl %ebx\n"
        }
        
      }
      case Struct(s, field) => {
        expandStruct(s, field, blockNr)
      }
      case StructApp(s, EApp(fun, l)) => {
        println("StructApp", s, fun)
        val adrress = getAdrress(Ident(s), blockNr)
        if (adrress != -1) {
          for (e <- l) { expr(e,blockNr); assCode += "pushl %eax\n"; }
          assCode += "movl " + adrress + "(%ebp), %eax\n"
          assCode += "pushl %eax\n"
          assCode += "call " + varTypes.get(s).get + fun.getName() + "\n"
          assCode += "pop %ebx\n"
          for (e <- l) { assCode += "popl %ebx\n"; }
        }
        else { 
          println("StructAppFun", s, fun)
          val typ = structTypes.get(currentStructId).get.get(s).get
          for (e <- l) { expr(e,blockNr); assCode += "pushl %eax\n"; }
          getStructField(currentStructId, Ident(s), false)
          assCode += "pushl %eax\n"
          assCode += "call " + typ + fun.getName() + "\n"
          assCode += "pop %ebx\n"
          for (e <- l) { assCode += "popl %ebx\n"; }
        } 
      }
      case RTable(_, e) => {
        expr(e, blockNr)
      }
      case RStruct(s) => {
        val size = getStructSize(s)
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
      case EApp(Struct(s, fun), l) => //never using?
        
      case EApp(i, l) => 
        for (e <- l) { expr(e,blockNr); assCode += "pushl %eax\n"; }
        assCode += "call " + i.getName() + "\n"
        for (e <- l) { assCode += "popl %ebx\n"; }
//        assCode += getMethodName(i)
      case EAdd(e1, e2) =>
        basicOperation(e1, e2, blockNr)
        
  /*      Checker.variables = checkerVariables
        if (Checker.getType(e1, blockNr) == TType("string")) {
          assCode += "pushl %eax\n"
          assCode += "pushl %ebx\n"
          assCode += "call concat\n"
          assCode += "popl %ebx\n"
          assCode += "popl %ebx\n"
        } 
        else*/ {
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
        basicOperation(e2,e1,blockNr)
        assCode += "movl $0, %edx\n"
        assCode += "idivl %ebx\n"
        assCode += "mov %edx, %eax\n"
      case EUMinus(e) => expr(e, blockNr); assCode += "negl %eax\n"
      case EUNeg(e) => {
        expr(e, blockNr)
        assCode += "cmpl $0, %eax\n"
        assCode += "sete %al\n"
        assCode += "movzbl %al, %eax\n"
      }
      case ECast(_) => {
        assCode += "movl $0, %eax\n"
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

  def putVariable(idd : LeftVar, blockNr : Int, typ : Type) = {
    val id : LeftVar = Ident(idd.getName())
    println ("put variable", id, blockNr)
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
//    Checker.check(tokens)
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
