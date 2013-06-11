
package math.ext
import  math.exp._
import math.util.Text

//import  core.topo.{ Hold, Hode }

trait Simplify
{
  self:Exp =>
  
  def sim : Exp = sim(this)
  
  def sim( exp:Exp ) : Exp = exp match
  {
    case Num(n)    => exp
    case Dbl(d)    => dbl(d)
    case Rat(n,d)  => rat(n,d)
    case Var(s)    => exp
    case Add(u,v)  => add(noparen(u),noparen(v))
    case Sub(u,v)  => sub(noparen(u),noparen(v))
    case Mul(u,v)  => mul(noparen(u),noparen(v))
    case Div(u,v)  => div(noparen(u),noparen(v))    
    case Pow(u,v)  => pow(noparen(u),noparen(v))
    case Rec(u)    => rec(u)
    case Neg(u)    => Neg(sim(u))
    case Abs(u)    => Abs(sim(u))
    case Par(u)    => Par(sim(u))
    case Brc(u)    => Brc(sim(u))
    case Lnn(u)    => Lnn(sim(u))
    case Log(u,r)  => Log(sim(u),r)
    case Roo(u,r)  => Roo(sim(u),r)
    case Eee(u)    => Eee(sim(u))
    case Sqt(u)    => Sqt(sim(u))
    case Sin(u)    => Sin(sim(u))
    case Cos(u)    => Cos(sim(u))
    case Tan(u)    => Tan(sim(u))
    case Csc(u)    => Csc(sim(u))
    case Sec(u)    => Sec(sim(u))
    case Cot(u)    => Cot(sim(u))
    case ASin(u)   => ASin(sim(u))
    case ACos(u)   => ACos(sim(u))
    case ATan(u)   => ATan(sim(u))
    case ACsc(u)   => ACsc(sim(u))
    case ASec(u)   => ASec(sim(u))
    case ACot(u)   => ACot(sim(u))
    case Equ(u,v)  => equ(u,v)
    case Dif(u)    => Dif(sim(u))
    case Sus(u,v)  => Sus(sim(u),sim(v))
    case Sup(u,v)  => Sup(sim(u),sim(v))
    case Lim(u,v)  => Lim(sim(u),sim(v))
    case Itg(u)    => Itg(sim(u)) 
    case Itl(a,b,u)=> Itl(sim(a),sim(b),sim(u))  
    case Sum(a,b,u)=> Sum(sim(a),sim(b),sim(u))   
    case Cex(r,i)  => Cex( r.sim, i.sim )
    case Vex(a)    => Vex(exp).map( a => a.sim )
    case Mex(m)    => Mex(exp).map( m => m.sim )
    case Msg(t)    => Msg(t)
  }

  def dbl( r:Double ) : Exp = if(r==r.toInt) Num(r.toInt) else Dbl(r)

  def rat( n:Int, d:Int ) : Exp = (n,d) match
  {
    case( 0, d1 ) => Num(0)
    case( n1, 0 ) => Msg( Text("Num(n1)/Num(0)") )
    case _        => if(n==d) 1 else Rat(n,d)
  }

  def mul( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( u1, Num(1)|Dbl(1.0) ) => sim(u1)
    case( Num(1)|Dbl(1.0), v1 ) => sim(v1)
    case( Num(a),   Num(b)    ) => Num(a*b)
    case( Num(a),   Dbl(b)    ) => Dbl(a*b)
    case( Dbl(a),   Num(b)    ) => Dbl(a*b)
    case( Dbl(a),   Dbl(b)    ) => Dbl(a*b)
    case( q,        Add(a,b)  ) => sim( sim(q)*sim(a)+sim(q)*sim(b) )
    case( Add(a,b), r         ) => sim( sim(a)*sim(r)+sim(b)*sim(b) )
    case( q,        Sub(a,b)  ) => sim( sim(q)*sim(a)-sim(u)*sim(b) )
    case( Sub(a,b), r         ) => sim( sim(a)*sim(r)-sim(b)*sim(b) )
    case _                      => sim(u)*sim(v)
  }

  def div( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( q, Num(1)|Dbl(1.0) ) => sim(q)
    case( q, Num(0)|Dbl(0.0) ) => Msg(Text("u/Num(0)"))
    case( Num(a),   Num(b)   ) => Rat(a,b)
    case( Num(a),   Dbl(b)   ) => Dbl(a/b)
    case( Dbl(a),   Num(b)   ) => Dbl(a/b)
    case( Dbl(a),   Dbl(b)   ) => Dbl(a/b) 
    case( Mul(a,b), r        ) => factor(u,r)
    case( q, Mul(a,b)        ) => factor(q,v)
    case _ => if(u==v) 1 else sim(u)/sim(v)
  }

   def factor( u:Exp,  v:Exp ) : Exp = u

  def add( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( q, Num(0)|Dbl(0.0) ) => sim(q)
    case( Num(0)|Dbl(0.0), r ) => sim(r)
    case( Num(a),   Num(b)   ) => Num(a+b)
    case( Num(a),   Dbl(b)   ) => Dbl(a+b)
    case( Dbl(a),   Num(b)   ) => Dbl(a+b)
    case( Dbl(a),   Dbl(b)   ) => Dbl(a+b)     
    case( q,        Neg(b)   ) => sim( sim(q)-sim(b) )
    case _                     =>      sim(u)+sim(v)
  }

  def sub( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( q, Num(0)|Dbl(0.0) ) => sim(q)
    case( Num(0)|Dbl(0.0), r ) => sim( Neg(r) )
    case( Num(a),   Num(b)   ) => Num(a-b)
    case( Num(a),   Dbl(b)   ) => Dbl(a-b)
    case( Dbl(a),   Num(b)   ) => Dbl(a-b)
    case( Dbl(a),   Dbl(b)   ) => Dbl(a-b)    
    case( q,        Neg(b)   ) => sim( sim(q)+sim(b) )
    case _                     => sim(u)-sim(v)
  }

  def pow( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( q, Num(1)|Dbl(1.0) ) => sim(q)
    case( q, Num(0)|Dbl(0.0) ) => 1
    case( Num(1)|Dbl(1.0), r ) => 1
    case( Num(0)|Dbl(0.0), r ) => 0
    case( Num(a),   Num(b)   ) => Dbl(Math.pow(a,b))
    case( Num(a),   Dbl(b)   ) => Dbl(Math.pow(a,b))
    case( Dbl(a),   Num(b)   ) => Dbl(Math.pow(a,b))
    case( Dbl(a),   Dbl(b)   ) => Dbl(Math.pow(a,b))    
    case _                     => sim(u)~^sim(v)
  }

  def rec( u:Exp ) : Exp = u match
  {
    case Num(1) => 1
    case Num(0) => Msg(Text("Rec(Num(0))"))
    case Par(a) => Rec(sim(a))
    case _      => Rec(sim(u))
  }

  def equ( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case _ => Equ( sim(u), sim(v) )
  }
}
/*
   def factorH( u:Exp, v:Exp ) : Exp =
   {
    val uh = mulHold(u,Hold[Exp]) // Convert numerator   to mul list
    val vh = mulHold(v,Hold[Exp]) // Convert denumerator to mul list
    factorHold( uh, vh )
    val um = holdMul(uh.head,uh)
    val vm = holdMul(vh.head,vh)

    (um,vm) match // Numerator or denumerator have been factored to one
    {
      case( q, Num(1) ) => sim(q)
      case( Num(1), r ) => Rec(sim(r))
      case _            => sim(um)/sim(vm)
    }

   }

   def mulHold( exp:Exp, hold:Hold[Exp] ) : Hold[Exp] =
   {
     exp match
     {
       case Mul(u,v) => mulHold(u,hold); mulHold(v,hold)
       case _        => hold.add(exp)
     }
     hold
   }

  def holdMul( node:Hode[Exp], hold:Hold[Exp] ) : Exp =
  {
    if( hold.in(node) )
    {
      if( hold.in(node.next) )
        return Mul( node.data, holdMul(node.next,hold) )
      else
        return node.data
    }
    Num(1)  // hold has been factored out i.e. hold.isEmpty
  }

  def factorHold( uh:Hold[Exp], vh:Hold[Exp] )
  {
    val ui = uh.head
    while( uh.in(ui) )
    {
      //val un = ui.next
      val vi = vh.head
      while( vh.in(vi) )
      {
        //val vn = vi.next
        if( ui.data ==  vi.data )
          { uh.del(ui); vh.del(vi) }
        else
          factorPow(ui,vi)
        //vi = vn
      }
      //ui = un
    }
  }

  def factorPow( ui:Hode[Exp], vi:Hode[Exp] )
  {
    (ui.data,vi.data) match
    {
      case ( Pow(a,b), Pow(c,d) ) if a==c  => ui.data = pow(a,b-d); vi.data = Num(1)
      case _                               =>
    }
  }
*/