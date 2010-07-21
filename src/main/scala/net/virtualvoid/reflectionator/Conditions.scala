package net.virtualvoid.reflectionator

import scala.reflect._

trait Expression {
  def eval(arg: AnyRef): AnyRef
  def asString: String
  def pos: Int
  def subExpressions: List[(Int, Expression)]
}
trait ReflectedExpression[-T, +U] {
  def eval(arg: T): U
}

object Expression {
  def apply(repr: String, posi: Int, subs: List[(Int, Expression)])(evaluator: AnyRef => AnyRef) = 
    new Expression {
      def eval(arg: AnyRef): AnyRef = evaluator(arg)
      def asString: String = repr
      def pos = posi
      def subExpressions = subs
      override def toString = pos+": "+asString+"["+subExpressions.mkString(", ")+"]"
    }

  type Invocable = (AnyRef, List[AnyRef]) => AnyRef
  def intOp(f: (Int, Int) => Any): Invocable = 
    (recv, args) => f(recv.asInstanceOf[Int], args(0).asInstanceOf[Int]).asInstanceOf[AnyRef]
  implicit def callableMethod(m: java.lang.reflect.Method): (AnyRef, List[AnyRef]) => AnyRef = (recv, args) => m.invoke(recv, args:_*)
  def method(clazz: String, method: String, args: List[Type]): (AnyRef, List[AnyRef]) => AnyRef = { //java.lang.reflect.Method = {
    val cl = Reflect.cleanClass(clazz)
    val Int = classOf[Int]
    (cl, method) match {
      case (_, "$eq$eq") => classOf[Object].getMethod("equals", classOf[Object])
      case (Int, "$greater") => intOp(_ > _)
      case (Int, "$plus") => intOp(_ + _)
      case _ => cl.getMethod(method, args.map(Reflect.classFromType(_)):_*)
    }
  }
  def build[T, U](TheArg: String, argTpe: java.lang.Class[_], tree: Tree): Expression = tree match {
    case Literal(x: String) => Expression('"'+x+'"', 0, Nil) { _ => x }
    case Literal(x: AnyRef) => Expression(x.toString, 0, Nil) { _ => x }
    case Ident(LocalValue(_, TheArg, _)) => Expression(TheArg, 0, Nil){ x => x }
    case Apply(Select(from, Method(methodDesc, MethodType(formals, resTpe))), args) =>
      val fromExp = build(TheArg, argTpe, from)
      val argsExp = args map (build(TheArg, argTpe, _))
      val (clazz, methodName) = Reflect.splitFullMethodName(methodDesc)
      val m = method(clazz, methodName, formals.map(_.tpe))

      val reprStart = fromExp.asString+"."
      val offset = reprStart.length
      val reprRest = methodName+"("+argsExp.map(_.asString).mkString(", ")+")"
      def posOfExpr(lastExpr: List[(Int, Expression, Int)], exp: Expression): List[(Int, Expression, Int)] = lastExpr match {
	case (start, _, length)::rest => (start+length, exp, start+length+exp.asString.length+2)::lastExpr
      }    
      val poss = argsExp.foldLeft(List((-offset, fromExp, reprStart.length+methodName.length+1)))(posOfExpr)
      Expression(reprStart+reprRest, offset, poss map { case (a,b,c) => (a,b) } ) { x =>
        m(fromExp.eval(x), argsExp.map(_.eval(x)))
      }
  }
  def build[T, U](tree: Tree): Expression = tree match {
    case Function(List(LocalValue(NoSymbol,paramName,paramTpe)),body) => build[T,U](paramName, Reflect.classFromType(paramTpe), body)
  }
  def build[T, U](code: Code[T => U]): Expression = build(code.tree)

  def analyze(exp: Expression, arg: AnyRef): String = {
    /** Gets position, value pairs */
    def find(offset: Int)(exp: Expression): List[(Int, String)] = {
      val pos = offset + exp.pos
      (pos, exp.eval(arg).toString)::exp.subExpressions.flatMap(x => find(pos+x._1)(x._2))
    }
    /** Layouts the annotations onto lines */
    def layout(anns: List[(Int, String)]): List[String] = {
      def fillTo(sb: StringBuilder, pos: Int): StringBuilder = {
	while(sb.length <= pos) { sb append ' ' }
	sb
      }
      def layoutAnn(lines: List[StringBuilder], a: (Int, String)): List[StringBuilder] = {
	val (pos, text) = a
	lines match {
	  case cur::rest if cur.length < pos - 1 => fillTo(cur, pos - 1).append(text) :: rest
	  case cur::rest =>
	    fillTo(cur, pos)
	    if (cur(pos) == ' ')
	      cur(pos) = '|'
	    cur::layoutAnn(rest, a)
	  case Nil => List(fillTo(new StringBuilder, pos - 1).append(text))
	}
      }
      def pointerLine:String = {
	val sb = new StringBuilder
	for ( (pos, _) <- anns ) fillTo(sb, pos)(pos) = '|'
	sb.toString
      }

      val valueLines = anns.foldLeft(List[StringBuilder]())(layoutAnn) map (_.toString)
      pointerLine :: valueLines
    }
    val pos = find(0)(exp)
    //println(pos)
    layout(pos.sortBy(_._1)).mkString("\n")
  }
}

object Usage {
  def main(args: Array[String]) {
    val exp = Expression.build((x: String) => x.length + 12 * 3 > 128)//x concat "Test")
    //println(exp.eval("Test".asInstanceOf[AnyRef])+" exp: "+exp.asString)
    //println(exp)
    println(exp.asString)
    println(Expression.analyze(exp, "Test"))
  }
}
