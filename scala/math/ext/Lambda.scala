
package math.ext
import  math.exp._
import  math.util.Text

trait Lambda
{
  self:Exp =>
    
  def lambda( t:Text ) { lambda(t,this) }
  
  def lambda( t:Text, exp:Exp )
  {
    exp match
    {
      case Num(n)    => lambda( t, "Num", n.toString )
      case Dbl(r)    => lambda( t, "Dbl", r.toString )
      case Rat(n,d)  => lambda( t, "Rat", n.toString, d.toString )
      case Var(s)    => lambda( t, "Var", s )
      case Add(u,v)  => lambda( t, "Add", u, v )
      case Sub(u,v)  => lambda( t, "Sub", u, v )
      case Mul(u,v)  => lambda( t, "Mul", u, v )
      case Div(u,v)  => lambda( t, "Div", u, v )
      case Rec(u)    => lambda( t, "Rec", Num(1), u )
      case Pow(u,v)  => lambda( t, "Pow", u, v )
      case Neg(u)    => lambda( t, "Neg", u )
      case Abs(u)    => lambda( t, "Abs", u )
      case Par(u)    => lambda( t, "Par", u )
      case Brc(u)    => lambda( t, "Brc", u )
      case Lnn(u)    => lambda( t, "Lnn", u )
      case Log(u,b)  => lambda( t, "Log", u, b )
      case Roo(u,r)  => lambda( t, "Roo", u, r )
      case Eee(u)    => lambda( t, "Eee", u )
      case Sqt(u)    => lambda( t, "Sqt", u )
      case Sin(u)    => lambda( t, "Sin",  u )
      case Cos(u)    => lambda( t, "Cos",  u )
      case Tan(u)    => lambda( t, "Tan",  u )
      case Csc(u)    => lambda( t, "Csc",  u )
      case Sec(u)    => lambda( t, "Sec",  u )
      case Cot(u)    => lambda( t, "Cot",  u )
      case ASin(u)   => lambda( t, "ASin", u )
      case ACos(u)   => lambda( t, "ACos", u )
      case ATan(u)   => lambda( t, "ATan", u )
      case ACsc(u)   => lambda( t, "ACsc", u )
      case ASec(u)   => lambda( t, "ASec", u )
      case ACot(u)   => lambda( t, "ACot", u )
      case Dif(u)    => lambda( t, "Dif",  u )
      case Sus(u,v)  => lambda( t, "Sus",  u, v )
      case Sup(u,v)  => lambda( t, "Sup",  u, v )
      case Lim(u,v)  => lambda( t, "Lim",  u, v )
      case Itg(u)    => lambda( t, "Itg",  u )
      case Itl(a,b,u)=> lambda( t, "Itl",  a, b, u )
      case Sum(a,b,u)=> lambda( t, "Itg",  a, b, u )
      case Equ(u,v)  => lambda( t, "Equ",  u, v )
      case Cex(r,i)  => lambdaCex(t,r,i)
      case Vex(a)    => lambdaVex(t,a)
      case Mex(m)    => lambdaMex(t,m)
      case Msg(txt:Text) => t.app(txt)
    }
  }

  def lambda( t:Text, name:String, s:String )
    { t.app( name, '(', s, ')' ) }

  def lambda( t:Text, name:String, s1:String, s2:String )
    { t.app( name, '(', s1, ',', s2, ')' ) }

  def lambda( t:Text, name:String, u:Exp )
    { t.app( name, '(' ); u.lambda(t); t.app(')') }

  def lambda( t:Text, name:String, u:Exp, v:Exp )
    { t.app( name, '(' ); u.lambda(t); t.app(','); v.lambda(t); t.app(')') }
  
  def lambda( t:Text, name:String, a:Exp, b:Exp, u:Exp )
    { t.app( name, '(' ); a.lambda(t); t.app(','); b.lambda(t); t.app(','); u.lambda(t); t.app(')') }    
  
  def lambdaCex( t:Text, r:Exp, i:Exp )
    { t.app("Cex("); lambda(t,r); t.app(','); lambda(t,i); t.app(')') }  
  
  def lambdaVex( t:Text, a:Array[Exp] )
  { 
    t.app("Vex("); a(0).lambda(t)
    for( i <- 1 until a.length ) 
      { t.app(','); a(i).lambda(t) }
    t.app( ')' )
  }
  
  def lambdaMex( t:Text, mat:Array[Vex] )
  { 
    t.app( "Mex(", mat(0).lambda(t) ) 
    for( i <- 1 until mat.length )
      { t.app(','); mat(i).lambda(t) }
    t.app(')') 
  }
}