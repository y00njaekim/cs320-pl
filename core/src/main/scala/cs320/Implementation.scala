package cs320

import Value._

object Implementation extends Template {
  
  def lookup(name: String, env: Env): Value =
    env.get(name) match {
      case Some(v) => v
      case None => error(s"free identifier: $name")
    }

  def numOp(op: (BigInt, BigInt) => BigInt): (Value, Value) => Value =
    (_, _) match {
      case (IntV(x), IntV(y)) => IntV(op(x, y))
      case (x, y) => error(s"not both numbers; $x, $y")
    }
  
  val numVAdd = numOp(_ + _)
  val numVMul = numOp(_ * _)

  def numOp4Div(op: (BigInt, BigInt) => BigInt): (Value, Value) => Value =
    (_, _) match {
      case (IntV(x), IntV(y)) => {
        if(y != 0) {
          IntV(op(x, y))
        } else {
          error(s"second arguments should not be zero;")
        }
      }
      case (x, y) => error(s"not both numbers; $x, $y")
    }
  val numVDiv = numOp4Div(_ / _)
  val numVMod = numOp4Div(_ % _)

  def numOp4Cmp(op: (BigInt, BigInt) => Boolean): (Value, Value) => Value =
    (_, _) match {
      case (IntV(x), IntV(y)) => BooleanV(op(x, y))
      case (x, y) => error(s"not both numbers; $x, $y")
    }

  val numVEql = numOp4Cmp(_ == _)
  val numVLess = numOp4Cmp(_ < _)



  def interp(expr: Expr): Value = {
    def listInterp(l: List[Expr], env: Env): List[Value] = l match {
      case h :: t => {
        val hV = myInterp(h, env)
        val tV = listInterp(t, env)
        hV :: tV
      }
      case Nil => Nil
    }
    def myInterp(e: Expr, env: Env): Value = e match {
      // variable
      case Id(name) => lookup(name, env)
      // integer
      case IntE(value) => IntV(value)
      // boolean
      case BooleanE(value) => BooleanV(value)
      // addition
      case Add(left, right) => numVAdd(myInterp(left, env), myInterp(right, env))
      // multiplication
      case Mul(left, right) => numVMul(myInterp(left, env), myInterp(right, env))
      // division
      case Div(left, right) => numVDiv(myInterp(left, env), myInterp(right, env))
      // modulo
      case Mod(left, right) =>numVMod(myInterp(left, env), myInterp(right, env))
      // equal-to
      case Eq(left: Expr, right: Expr) => numVEql(myInterp(left, env), myInterp(right, env))
      // less-then
      case Lt(left: Expr, right: Expr) => numVLess(myInterp(left, env), myInterp(right, env))
      // conditional
      case If(condition: Expr, trueBranch: Expr, falseBranch: Expr) => myInterp(condition, env) match {
        case BooleanV(b) => {
          if(b) {
            myInterp(trueBranch, env)
          } else {
            myInterp(falseBranch, env)
          }
        }
        case b => error(s"condition should be boolean;")
      }
      // tuple
      case TupleE(expressions: List[Expr]) => expressions match {
        case Nil => NilV
        case l => TupleV(listInterp(expressions, env))
      }
      // projection
      case Proj(expression: Expr, index: Int) => {
        val eV = myInterp(expression, env)
        val res = eV match {
          case TupleV(l) => l(index-1)
        }
        res
      }
      // nil
      case  NilE => NilV
      // cons
      case ConsE(head: Expr, tail: Expr) => {
        val hV = myInterp(head, env)
        val tV = myInterp(tail, env)
        ConsV(hV, tV)
      }
      // is-empty
      case Empty(expression: Expr) => myInterp(expression, env) match {
        case NilV => BooleanV(true)
        case ConsV(h, t) => BooleanV(false)
        case e => error(s"expression should be either empty list or nonempty list")
      }
      // head
      case Head(expression: Expr) => myInterp(expression, env) match {
        case ConsV(h, t) => h
        case e => error(s"expression should be nonempty list")
      }
      // tail
      case Tail(expression: Expr) => myInterp(expression, env) match {
        case ConsV(h, t) => t
        case e => error(s"expression should be nonempty list")
      }
      // local variable
      case Val(name: String, expression: Expr, body: Expr) => {
        val v = myInterp(expression, env)
        val nenv = env + (name -> v)
        val res = myInterp(body, nenv)
        res
      }
      // anonymous function
      case Fun(parameters: List[String], body: Expr) => {
        CloV(parameters, body, env)
      }
      // recursive function
      case RecFuns(functions: List[FunDef], body: Expr) => {
        var nenv : Env = Map[String, Value]()
        for(e <- functions) {
          nenv = nenv + (e.name -> CloV(e.parameters, e.body, nenv))
        }
        myInterp(body, nenv)
      }
      // function application
      case App(function: Expr, arguments: List[Expr]) => myInterp(function, env) match {
        case CloV(p, b, e) => {
          if(p.length != arguments.length) {
            error(s"arguments length should be equal with CloV's parameter")
          }
          var nenv : Env = e
          for(i <- 1 to arguments.length) {
            nenv = nenv + (p(i-1) -> myInterp(arguments(i-1), env))
          }
          myInterp(b, nenv)
        }
        case v => error(s"expression should be interpreted to closure")
      }
      // type test
      case Test(expression: Expr, typ: Type) => {
        val res = myInterp(expression, env) match {
          case IntV(v) => typ match {
            case IntT => BooleanV(true)
            case t => BooleanV(false)
          }
          case BooleanV(v) => typ match {
            case BooleanT => BooleanV(true)
            case t => BooleanV(false)
          }
          case TupleV(v) => typ match {
            case TupleT => BooleanV(true)
            case t => BooleanV(false)
          }
          case NilV => typ match {
            case ListT => BooleanV(true)
            case t => BooleanV(false)
          }
          case ConsV(h, t) => typ match {
            case ListT => BooleanV(true)
            case t => BooleanV(false)
          }
          case CloV(p, b, e) => typ match {
            case FunctionT => BooleanV(true)
            case t => BooleanV(false)
          }
        }
        res
      }
    }
    val myEnv : Env = Map[String, Value]()
    myInterp(expr, myEnv)
  }
}
