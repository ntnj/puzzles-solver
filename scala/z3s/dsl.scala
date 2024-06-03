package z3s

import scala.language.implicitConversions
import language.experimental.into
import com.microsoft.z3
import scala.annotation.targetName
import scala.compiletime.erasedValue

given (using ctx: z3.Context): Conversion[Int, z3.IntExpr] = ctx.mkInt(_)
given (using ctx: z3.Context): Conversion[Boolean, z3.BoolExpr] = ctx.mkBool(_)
given (using ctx: z3.Context): Conversion[String, z3.Symbol] = ctx.mkSymbol(_)
given [T]: Conversion[T, T] = identity

type Sort[T] <: z3.Sort = T match
  case Int     => z3.IntSort
  case Boolean => z3.BoolSort
  case BV[t]   => z3.BitVecSort

type Expr[T] = T match
  case BV[t] => BV[t]
  case _     => z3.Expr[Sort[T]]

trait Func[I <: Tuple, O](f: z3.FuncDecl[Sort[O]]):
  inline def decl: z3.FuncDecl[Sort[O]] = f

private case class Func1[I <: Tuple, O](f: z3.FuncDecl[Sort[O]]) extends Func[I, O](f):
  inline def apply(i0: Expr[Tuple.Elem[I, 0]])(using z3.Context, Conversion[Expr[Tuple.Elem[I, 0]], z3.Expr[?]]): Expr[O] = wrapExpr(f(i0))

private case class Func2[I <: Tuple, O](f: z3.FuncDecl[Sort[O]]) extends Func[I, O](f):
  inline def apply(i0: Expr[Tuple.Elem[I, 0]], i1: Expr[Tuple.Elem[I, 1]])(using
      z3.Context,
      Conversion[Expr[Tuple.Elem[I, 0]], z3.Expr[?]],
      Conversion[Expr[Tuple.Elem[I, 1]], z3.Expr[?]]
  ): Expr[O] = wrapExpr(f(i0, i1))

type TupleSort[T <: Tuple] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case BV[i] *: t => BV[i] *: TupleSort[t]
  case h *: t     => z3.Expr[Sort[h]] *: TupleSort[t]

transparent inline def sort[T](using ctx: z3.Context): Sort[T] = inline erasedValue[T] match
  case _: Int     => ctx.getIntSort()
  case _: Boolean => ctx.getBoolSort()
  case _: BV[t]   => ctx.mkBitVecSort(valueOf[t])

inline def tupleSort[T <: Tuple](using ctx: z3.Context): Array[z3.Sort] =
  inline erasedValue[T] match
    case _: EmptyTuple => Array()
    case _: (h *: t)   => sort[h] +: tupleSort[t]

inline def int(s: z3.Symbol)(using ctx: z3.Context): z3.IntExpr = ctx.mkIntConst(s)
inline def bool(s: z3.Symbol)(using ctx: z3.Context): z3.BoolExpr = ctx.mkBoolConst(s)
transparent inline def func[A <: Tuple, T](s: z3.Symbol)(using ctx: z3.Context): Func[A, T] = inline compiletime.constValue[Tuple.Size[A]] match
  case 1 => Func1(ctx.mkFuncDecl(s, tupleSort[A], sort[T]))
  case 2 => Func2(ctx.mkFuncDecl(s, tupleSort[A], sort[T]))

transparent inline def freshExpr[T](prefix: String)(using ctx: z3.Context): Expr[T] = inline erasedValue[T] match
  case _: BV[t] => BV[t](ctx.mkFreshConst(prefix, sort[BV[t]]))
  case _        => ctx.mkFreshConst(prefix, sort[T])

transparent inline def wrapExpr[T](e: z3.Expr[Sort[T]])(using ctx: z3.Context): Expr[T] = inline erasedValue[T] match
  case _: BV[t] => BV[t](compiletime.summonInline[Conversion[z3.Expr[Sort[T]], z3.Expr[z3.BitVecSort]]](e))
  case _        => e

inline def forall[T](inline f: Expr[T] => z3.Expr[z3.BoolSort])(using ctx: z3.Context)(using Conversion[Expr[T], z3.Expr[?]]) =
  val x = freshExpr[T]("_x")
  ctx.mkForall(Array(x), f(x), 0, null, null, null, null)
inline def forall[T, U](inline f: (Expr[T], Expr[U]) => z3.Expr[z3.BoolSort])(using
    ctx: z3.Context
)(using Conversion[Expr[T], z3.Expr[?]], Conversion[Expr[U], z3.Expr[?]]) =
  val x0 = freshExpr[T]("_x")
  val x1 = freshExpr[U]("_x")
  ctx.mkForall(Array(x0, x1), f(x0, x1), 0, null, null, null, null)

inline def distinct(args: z3.Expr[?]*)(using ctx: z3.Context) = ctx.mkDistinct(args*)
inline def distinct[T <: Int](args: BV[T]*)(using ctx: z3.Context) = ctx.mkDistinct(args.map(_.bv)*)
inline def sum[T <: z3.ArithSort](args: z3.Expr[T]*)(using ctx: z3.Context) = ctx.mkAdd(args*)
inline def sum(args: z3.Expr[z3.BitVecSort]*)(using ctx: z3.Context) = args.reduce(ctx.mkBVAdd(_, _))
inline def sum[T <: Int](args: BV[T]*)(using ctx: z3.Context) = args.reduce((a, b) => BV[T](ctx.mkBVAdd(a.bv, b.bv)))

case class BV[T <: Int](bv: z3.Expr[z3.BitVecSort])(using ctx: z3.Context):
  @targetName("eq")
  inline def ===(r: BV[T]): z3.BoolExpr = ctx.mkEq(bv, r.bv)
  @targetName("ne")
  inline def !==(r: BV[T]): z3.BoolExpr = ctx.mkNot(ctx.mkEq(bv, r.bv))
  @targetName("minus")
  inline def unary_- : z3.BitVecExpr = ctx.mkBVNeg(bv)
  @targetName("add")
  inline def +(r: BV[T]): z3.BitVecExpr = ctx.mkBVAdd(bv, r.bv)
  @targetName("sub")
  inline def -(r: BV[T]): z3.BitVecExpr = ctx.mkBVSub(bv, r.bv)
  @targetName("mul")
  inline def *(r: BV[T]): z3.BitVecExpr = ctx.mkBVMul(bv, r.bv)
  @targetName("div")
  inline def /(r: BV[T]): z3.BitVecExpr = ctx.mkBVSDiv(bv, r.bv)
  @targetName("lt")
  inline def <(r: BV[T]): z3.BoolExpr = ctx.mkBVSLT(bv, r.bv)
  @targetName("le")
  inline def <=(r: BV[T]): z3.BoolExpr = ctx.mkBVSLE(bv, r.bv)
  @targetName("gt")
  inline def >(r: BV[T]): z3.BoolExpr = ctx.mkBVSGT(bv, r.bv)
  @targetName("ge")
  inline def >=(r: BV[T]): z3.BoolExpr = ctx.mkBVSGE(bv, r.bv)

object BV:
  given [T <: Int](using ctx: z3.Context, v: ValueOf[T]): Conversion[Int, BV[T]] = i => BV[T](ctx.mkBV(i, v.value))
  given [T <: Int](using ctx: z3.Context): Conversion[BV[T], z3.Expr[z3.BitVecSort]] = _.bv

extension [T <: z3.Sort](inline l: z3.Expr[T])(using ctx: z3.Context)
  @targetName("eq")
  inline def ===(r: z3.Expr[T]): z3.BoolExpr = ctx.mkEq(l, r)
  @targetName("ne")
  inline def !==(r: z3.Expr[T]): z3.BoolExpr = ctx.mkNot(ctx.mkEq(l, r))

extension [T <: z3.ArithSort](inline l: z3.Expr[T])(using ctx: z3.Context)
  @targetName("minus")
  inline def unary_- : z3.ArithExpr[T] = ctx.mkUnaryMinus(l)
  @targetName("add")
  inline def +(r: z3.Expr[T]): z3.ArithExpr[T] = ctx.mkAdd(l, r)
  @targetName("sub")
  inline def -(r: z3.Expr[T]): z3.ArithExpr[T] = ctx.mkSub(l, r)
  @targetName("mul")
  inline def *(r: z3.Expr[T]): z3.ArithExpr[T] = ctx.mkMul(l, r)
  @targetName("div")
  inline def /(r: z3.Expr[T]): z3.ArithExpr[T] = ctx.mkDiv(l, r)
  @targetName("power")
  inline def **(r: z3.Expr[T]): z3.ArithExpr[T] = ctx.mkPower(l, r)
  @targetName("lt")
  inline def <(r: z3.Expr[T]): z3.BoolExpr = ctx.mkLt(l, r)
  @targetName("le")
  inline def <=(r: z3.Expr[T]): z3.BoolExpr = ctx.mkLe(l, r)
  @targetName("gt")
  inline def >(r: z3.Expr[T]): z3.BoolExpr = ctx.mkGt(l, r)
  @targetName("ge")
  inline def >=(r: z3.Expr[T]): z3.BoolExpr = ctx.mkGe(l, r)

extension (inline l: z3.Expr[z3.BoolSort])(using ctx: z3.Context)
  @targetName("not")
  inline def unary_! : z3.BoolExpr = ctx.mkNot(l)
  @targetName("and")
  inline def &&(r: z3.Expr[z3.BoolSort]): z3.BoolExpr = ctx.mkAnd(l, r)
  @targetName("or")
  inline def ||(r: z3.Expr[z3.BoolSort]): z3.BoolExpr = ctx.mkOr(l, r)
  @targetName("xor")
  inline def ^(r: z3.Expr[z3.BoolSort]): z3.BoolExpr = ctx.mkXor(l, r)
  @targetName("implies")
  inline def ->(r: z3.Expr[z3.BoolSort]): z3.BoolExpr = ctx.mkImplies(l, r)
  inline infix def iff(r: z3.Expr[z3.BoolSort]): z3.BoolExpr = ctx.mkIff(l, r)
  inline def ite[S <: z3.Sort](i: z3.Expr[? <: S], t: z3.Expr[? <: S]): z3.Expr[S] = ctx.mkITE(l, i, t)

extension (inline l: z3.Expr[z3.BitVecSort])(using ctx: z3.Context)
  @targetName("minus")
  inline def unary_- : z3.BitVecExpr = ctx.mkBVNeg(l)
  @targetName("add")
  inline def +(r: z3.Expr[z3.BitVecSort]): z3.BitVecExpr = ctx.mkBVAdd(l, r)
  @targetName("sub")
  inline def -(r: z3.Expr[z3.BitVecSort]): z3.BitVecExpr = ctx.mkBVSub(l, r)
  @targetName("mul")
  inline def *(r: z3.Expr[z3.BitVecSort]): z3.BitVecExpr = ctx.mkBVMul(l, r)
  @targetName("div")
  inline def /(r: z3.Expr[z3.BitVecSort]): z3.BitVecExpr = ctx.mkBVSDiv(l, r)
  @targetName("lt")
  inline def <(r: z3.Expr[z3.BitVecSort]): z3.BoolExpr = ctx.mkBVSLT(l, r)
  @targetName("le")
  inline def <=(r: z3.Expr[z3.BitVecSort]): z3.BoolExpr = ctx.mkBVSLE(l, r)
  @targetName("gt")
  inline def >(r: z3.Expr[z3.BitVecSort]): z3.BoolExpr = ctx.mkBVSGT(l, r)
  @targetName("ge")
  inline def >=(r: z3.Expr[z3.BitVecSort]): z3.BoolExpr = ctx.mkBVSGE(l, r)
