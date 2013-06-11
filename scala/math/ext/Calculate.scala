
package math.ext
import  math.exp._

trait Calculate
{
   self:Exp =>
   type Assign  = String => Double  
     
  val NaN : Double = Double.NaN
  val Ee  : Double = Math.E
  val Pi  : Double = Math.PI
   
  def calc( a:Assign ) : Double = calc(a,this)
    
  def calc( a:Assign, exp:Exp ) : Double = exp match
  {
    case Num(n)    => n.toDouble
    case Dbl(d)    => d
    case Rat(n,d)  => div(n,d)
    case Var(s)    => a(s)
    case Add(u,v)  => u.calc(a) + v.calc(a)
    case Sub(u,v)  => u.calc(a) - v.calc(a)
    case Mul(u,v)  => u.calc(a) * v.calc(a)
    case Div(u,v)  => div(a,u,v)
    case Rec(u)    => div(a,1,u)
    case Pow(u,v)  => pow( u.calc(a), v.calc(a) )
    case Neg(u)    => -u.calc(a)
    case Abs(u)    => abs(u.calc(a))
    case Par(u)    => u.calc(a)
    case Brc(u)    => u.calc(a)
    case Lnn(u)    => ln(   u.calc(a) )
    case Log(u,b)  => log(  u.calc(a), b.calc(a) )
    case Roo(u,r)  => root( u.calc(a), r.calc(a) )
    case Eee(u)    => eee(  u.calc(a)  )
    case Sqt(u)    => sqrt( u.calc(a) )
    case Sin(u)    => sin(  u.calc(a) )
    case Cos(u)    => cos(  u.calc(a) )
    case Tan(u)    => tan(  u.calc(a) )
    case Csc(u)    => csc(  u.calc(a) )
    case Sec(u)    => sec(  u.calc(a) )
    case Cot(u)    => cot(  u.calc(a) )
    case ASin(u)   => asin( u.calc(a) )
    case ACos(u)   => acos( u.calc(a) )
    case ATan(u)   => atan( u.calc(a) )
    case ACsc(u)   => acsc( u.calc(a) )
    case ASec(u)   => asec( u.calc(a) )
    case ACot(u)   => acot( u.calc(a) )
    case Equ(u,v)  => v.calc(a) // Just evaluate right hand side of equation
    case Dif(u)    => NaN
    case Itg(u)    => NaN
    case Itl(q,b,u)=> NaN
    case Sum(q,b,u)=> NaN
    case Cex(r,i)  => NaN
    case Vex(vec)  => NaN
    case Mex(mat)  => NaN
    case Msg(txt)  => NaN
  }

  def div( a:Assign, u:Exp, v:Exp ) : Double =
    { val d = v.calc(a); if( d==0.0 ) NaN else u.calc(a) / d }

  def div(  n:Int, d:Int ) : Double =
    { if(d==0) NaN else n.toDouble / d.toDouble }

  def div(  n:Double, d:Double ) : Double = if(d==0) NaN else n/d

  def pow(  d:Double, p:Double ) : Double = Math.pow( d, p )
  def root( d:Double, r:Double ) : Double = Math.pow( d, 1.0/r )
  def log(  d:Double, b:Double ) : Double = Math.log(d) / Math.log(b)
  def abs(  d:Double ) : Double = Math.abs(d)
  def ln(   d:Double ) : Double = Math.log(d)
  def eee(  d:Double ) : Double = Math.exp(d)
  def sqrt( d:Double ) : Double = Math.sqrt(d)
  def sin(  d:Double ) : Double = Math.sin(d)
  def cos(  d:Double ) : Double = Math.cos(d)
  def tan(  d:Double ) : Double = Math.tan(d)
  def csc(  d:Double ) : Double = 1.0 / Math.sin(d)
  def sec(  d:Double ) : Double = 1.0 / Math.cos(d)
  def cot(  d:Double ) : Double = 1.0 / Math.tan(d)
  def asin( d:Double ) : Double = Math.asin(d)
  def acos( d:Double ) : Double = Math.acos(d)
  def atan( d:Double ) : Double = Math.atan(d)
  def acsc( d:Double ) : Double = Math.asin(1.0/d)
  def asec( d:Double ) : Double = Math.acos(1.0/d)
  def acot( d:Double ) : Double = Math.atan(1.0/d)
  
}