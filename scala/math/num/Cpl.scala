
package math.num

class Cpl( _r:Double, _i:Double )
{
   val r:Double = _r
   val i:Double = _i

   def + ( b:Cpl ) : Cpl = new Cpl( r+b.r, i+b.i )
   def - ( b:Cpl ) : Cpl = new Cpl( r-b.r, i-b.i )
   def * ( b:Cpl ) : Cpl = new Cpl( r*b.r-i*b.i, r*b.i+b.r*i )
   def / ( b:Cpl ) : Cpl =
   {
      val s = 1.0/b.sq
      new Cpl( (r*b.r+i*b.i)*s , (i*b.r-r*b.i)*s )
   }

   def sq  : Double = r*r + i*i
   def mag : Double = Math.sqrt(sq)
   def ang : Double =
   { 
     val s = 1.0/mag; val cos = r*s
     if( i>0.0 ) Math.acos(cos) else Math.PI*2.0 - Math.acos(cos)
   } 

   def conj : Cpl = new Cpl( r, -i )
   def Unit : Cpl = { val s = 1.0/mag; new Cpl( r*s, i*s ) }
   def pow( n:Double ) : Cpl = 
   { 
      val a = ang; val p = Math.pow(mag,n)
      new Cpl( p*Math.cos(a), p*Math.sin(a) ) 
   }

   override def toString = "" + r + (if (i < 0) "" else "+") + i + "i"
}