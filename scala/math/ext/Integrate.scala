
package math.ext
import  math.exp._
import math.util.Text

trait Integrate
{
  self:Exp =>
  
  def itg : Exp = I(this)
  
  def I( exp:Exp ) : Exp = exp match
  {
    case Num(n)    => n  
    case Dbl(r)    => r 
    case Rat(n,d)  => n / d
    case Var(s)    => Var(s)~^2 / 2
    case Add(u,v)  => I(u) + I(v)
    case Sub(u,v)  => I(u) - I(v)
    case Mul(u,v)  => IMul(noparen(u),noparen(v))
    case Div(u,v)  => IDiv(noparen(u),noparen(v))
    case Pow(u,v)  => IPow(noparen(u),noparen(v))
    case Rec(u)    => Lnn(u)
    case Neg(u)    => -I(u)
    case Abs(u)    => Abs(I(u))
    case Par(u)    => I(u)
    case Brc(u)    => I(u)
    case Lnn(u)    => u * Lnn(u)     - u
    case Log(u,r)  => u * Log(u,r) - u
    case Roo(u,r)  => Par(r/(r+1)) * u~^((r+1)/r)
    case Eee(u)    => Eee(u)
    case Sqt(u)    => Rat(2,3) * u~^Rat(3,2) // ipow(u,Rat(1/2))
    case Sin(u)    => -Cos(u)
    case Cos(u)    =>  Sin(u)
    case Tan(u)    => -Lnn(Abs(Cos(u)))
    case Csc(u)    =>  Lnn(Abs(Csc(u)+Cot(u)))
    case Sec(u)    =>  Lnn(Abs(Sec(u)+Tan(u)))
    case Cot(u)    =>  Lnn(Abs(Sin(u)))
    case ASin(u)   =>  u * ASin(u) + Sqt( 1 - u~^2 )
    case ACos(u)   =>  u * ACos(u) - Sqt( 1 - u~^2 )
    case ATan(u)   =>  u * ATan(u) - Lnn( 1 + u~^2 ) / 2
    case ACot(u)   =>  u * ACot(u) + Lnn( 1 + u~^2 ) / 2
    case ACsc(u)   =>  u * ACsc(u) + Lnn( u + Sqt( u~^2 - 1 ) )
    case ASec(u)   =>  u * ASec(u) - Lnn( u + Sqt( u~^2 - 1 ) )
    case Equ(u,v)  => I(u) equ I(v)
    case Dif(u)    => u
 // case Sus(u,v)  => Itg(Sub(u,v))
 // case Sup(u,v)  => Itg(Sup(u,v))
 // case Lim(u,v)  => Itg(Lim(u,v))     
    case Itg(u)    => I(Itg(u))
    case Itl(a,b,u)=> Itl(a,b,I(u))
    case Sum(a,b,u)=> Sum(a,b,I(u))     
    case Cex(r,i)  => Cex( r.itg, i.itg )
    case Vex(a)    => Vex(exp).map( a => a.itg )
    case Mex(m)    => Mex(exp).map( m => m.itg )
    case Msg(txt:Text)  => Itg(Msg(txt))
  }

  def IMul( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case ( u1, Num(n) ) => Num(n) * I(u1)
    case ( u1, Dbl(r) ) => Dbl(r) * I(u1)
    case ( Num(n), v1 ) => Num(n) * I(v1)
    case ( Dbl(r), v1 ) => Dbl(r) * I(v1)
    case _              => IPart(u,v)
  }
  
  def IDiv( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case ( u1, Num(n) ) => I(u1) / n
    case ( u1, Dbl(r) ) => I(u1) / r
    case ( Num(n), v1 ) => n * Lnn(v1)
    case ( Dbl(r), v1 ) => r * Lnn(v1)
    case _              => Not(Itg(Div(u,v)))
  }  
  
  def IPow( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case ( u1, Num(n) ) => u1~^(n+1)  / (n+1)
    case ( u1, Dbl(r) ) => u1~^(r+1)  / (r+1)
    case ( Num(n), v1 ) => Num(n)~^v1 / ln(n)
    case ( Dbl(r), v1 ) => Dbl(r)~^v1 / ln(r)
    case _              => Not(Itg(Pow(u,v)))
  }  
  
// Integration by parts
// I(u*d(v)) = u*v - I(v*d(u))
// I(v*d(u)) = u*v - I(u*d(v))
   def IPart( u:Exp, v:Exp ) : Exp = Not(Itg(Mul(u,v)))

}