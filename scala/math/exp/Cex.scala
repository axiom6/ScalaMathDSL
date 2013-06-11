
package math.exp
import  math.num.Cpl
import  math.util.{ Log => Logg }

case class Cex( r:Exp, i:Exp ) extends Exp
{  
   def this( cex:Cex ) = this(cex.r,cex.i) // Copy constructor

   def + ( b:Cex ) : Cex = new Cex( Add(r,b.r), Add(i,b.i) )
   def - ( b:Cex ) : Cex = new Cex( Sub(r,b.r), Sub(i,b.i) )

   def / ( b:Cex ) : Cex =
   {
      val s = Rec(b.sq)
      new Cex( Mul(Add(Mul(r,b.r),Mul(i,b.i)),s),
               Mul(Sub(Mul(i,b.r),Mul(r,b.i)),s) )
   }

   def *   ( b:Cex ) : Cex =  // Cex * Cex
     { new Cex( Sub(Mul(r,b.r),Mul(i,b.i)), Add(Mul(r,b.i),Mul(b.r,i)) )  }                                           

    override def *   ( b:Exp ) : Cex = new Cex( Mul(r,b), Mul(i,b) ) // Cex * Exp
             def mul ( b:Exp ) : Cex = new Cex( Mul(b,r), Mul(b,r) ) // Exp * Cex

   def sq  : Exp = Add(Mul(r,r),Mul(i,i))
   def mag : Exp = Sqt(sq)
   def ang : Exp =
   { 
     val s = Rec(mag); val cos = Mul(r,s)
  // if( i.eval>0.0 ) ArcCos(cos) else Sub(Num(Math.Pi*2.0),ArcCos(cos))
     ACos(cos)
   } 

   def conj : Cex = new Cex( r, Neg(i) )
   def Unit : Cex = { val s = Rec(mag); new Cex( Mul(r,s), Mul(i,s) ) }
   def pwr( n:Double ) : Cex = 
   { 
      val a = ang; val p = Pow(mag,Dbl(n))
      new Cex( Mul(p,Cos(a)), Mul(p,Sin(a)) ) 
   }

  def calcCex( aa:Assign ) : Cpl = new Cpl( calc(aa,r), calc(aa,r) )

}

object Cex
{  
  val imag:Cex = new Cex(Num(0),Num(1))
  val emp :Cex = new Cex(Num(0),Num(0))
  
  implicit def dbl2Cex( x:Double ) : Cex = Cex( Dbl(x), Num(0) )

   def apply( exp:Exp ) : Cex = exp match  // This a cast
   {
     case cex:Cex  => cex
     case _        => Logg.trace(4, "Bad Cast", exp.toString); emp
   }   

}
