
package math.exp
import  math.ext._
import  math.util.Text

abstract class Exp extends
  Ascii     with Lambda        with MathML    with Latex      with
  Calculate with Differentiate with Integrate with Precedence with Simplify
{
  type dbl     = Double

  implicit def Int2Num( n:Int    ) : Exp = Num(n)
  implicit def dbl2Dbl( r:Double ) : Exp = Dbl(r)
  //implicit def str2Var( s:String ) : Exp = Var(s)

  // ... arithmetic operators from low to high precedence ....

  def equ ( v:Exp ) : Exp = Equ(this,v)
  def +   ( v:Exp ) : Exp = Add(this,v)
  def -   ( v:Exp ) : Exp = Sub(this,v)
  def *   ( v:Exp ) : Exp = Mul(this,v)
  def /   ( v:Exp ) : Exp = Div(this,v)
  def ~^  ( v:Exp ) : Exp = Pow(this,v) // ~^is used instead of ^ for highest precedence
  def unary_-       : Exp = Neg(this)

  def noparen( e:Exp ) : Exp = e match
  {
    case Par(u)   => noparen(u)
    case Brc(u)   => noparen(u)
    case Mul(u,v) => Mul(noparen(u),noparen(v))
    case Div(u,v) => Div(noparen(u),noparen(v))
    case _        => e
  }

  def noparen : Exp = noparen(this)

  def text:Text = { val t = Text(50); ascii(t,this); t }
  override def toString : String = text.toString

}

// Numbers and Variables
case class Num( n:Int )          extends Exp // n Int
case class Dbl( r:Double )       extends Exp // r Double 
case class Rat( n:Int, r:Int )   extends Exp // n / r
case class Var( s:String )       extends Exp // s String

// Binary Operators from lowest to highest precedence
case class Equ( u:Exp, v:Exp ) extends Exp // u = v  Equation
case class Add( u:Exp, v:Exp ) extends Exp // u + v
case class Sub( u:Exp, v:Exp ) extends Exp // u - v
case class Mul( u:Exp, v:Exp ) extends Exp // u * v
case class Div( u:Exp, v:Exp ) extends Exp // u / v
case class Pow( u:Exp, v:Exp ) extends Exp // u ^ v

// Unary operator high precendence
case class Neg( u:Exp )        extends Exp // -u
case class Rec( u:Exp )        extends Exp // 1 / u

// Parenthesis Braces and Absolute Value
case class Par( u:Exp )        extends Exp // (u)
case class Brc( u:Exp )        extends Exp // {u}
case class Abs( u:Exp )        extends Exp // |u|

// Natural Log, Log Base, Root, Square Root and e
case class Lnn( u:Exp )         extends Exp // ln(u)
case class Log( u:Exp, r:Dbl )  extends Exp // log_b(u)
case class Roo( u:Exp, r:Dbl )  extends Exp // root_b(u)
case class Sqt( u:Exp )         extends Exp // sqrt(u)
case class Eee( u:Exp )         extends Exp // e^u

// Trigometric
case class Sin( u:Exp ) extends Exp
case class Cos( u:Exp ) extends Exp
case class Tan( u:Exp ) extends Exp
case class Sec( u:Exp ) extends Exp
case class Csc( u:Exp ) extends Exp
case class Cot( u:Exp ) extends Exp

// Inverse Trigometric
case class ASin( u:Exp ) extends Exp
case class ACos( u:Exp ) extends Exp
case class ATan( u:Exp ) extends Exp
case class ASec( u:Exp ) extends Exp
case class ACsc( u:Exp ) extends Exp
case class ACot( u:Exp ) extends Exp

// Differentiation, Limits, Integration, Summation and Integration with Lim
case class Dif( u:Exp )        extends Exp // d(u)   Differentiate
case class Lim( a:Exp, b:Exp ) extends Exp // _a^b limits for Sum and Int
case class Itg( u:Exp )        extends Exp // itg(u) Intergrate
case class Sum( a:Exp, b:Exp, u:Exp ) extends Exp
case class Itl( a:Exp, b:Exp, u:Exp ) extends Exp

// Subscript Superscipt Simplify Not are suspect
// Msg Err are also a minor concern
case class Sus( u:Exp, v:Exp ) extends Exp // x_1    Subscript   x1 -- place in Var
case class Sup( u:Exp, v:Exp ) extends Exp // x^1    Superscript x1 -- not used ^ => Pow
case class Not( u:Exp )        extends Exp // not an expression or can not be Intergrated
case class Sim( u:Exp )        extends Exp // sim(u) Simplify - just used for errors
case class Msg( m:Text )       extends Exp // error message from Parse
//case class Err( u:Exp )        extends Exp // error funtion on expression
//case class Fun( s:String, u:Exp ) extends Exp

