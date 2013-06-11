package algebra

sealed abstract class Exp extends  Calculate with Differentiate
{
  // Wrap i:Int and d:Double to Num(d) & String to Var(s)
  implicit def int2Exp( i:Int    ) : Exp = Num(i.toDouble)
  implicit def dbl2Exp( d:Double ) : Exp = Num(d)
  implicit def str2Exp( s:String ) : Exp = Var(s)

  // Infix operators from high to low using Scala precedence
  def ~^ ( v:Exp ) : Exp = Pow(this,v) // ~^ high precedence
  def /  ( v:Exp ) : Exp = Div(this,v)
  def *  ( v:Exp ) : Exp = Mul(this,v)
  def -  ( v:Exp ) : Exp = Sub(this,v)
  def +  ( v:Exp ) : Exp = Add(this,v)

  // Prefix operator for negation
  def unary_- : Exp = Neg(this)
}

case class Num( n:Double )     extends Exp // wrap double
case class Var( s:String )     extends Exp // wrap String
case class Par( u:Exp )        extends Exp // parentheses
case class Neg( u:Exp )        extends Exp // -u prefix
case class Pow( u:Exp, v:Exp ) extends Exp // u ^ v infix
case class Mul( u:Exp, v:Exp ) extends Exp // u * v infix
case class Div( u:Exp, v:Exp ) extends Exp // u / v infix
case class Add( u:Exp, v:Exp ) extends Exp // u + v infix
case class Sub( u:Exp, v:Exp ) extends Exp // u – v infix
case class Dif( u:Exp )        extends Exp // Differentiate
