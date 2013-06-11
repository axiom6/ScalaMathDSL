
package math.exp
import  math.num.Vec
import  math.util.{  Log=>Logg }

case class Vex( a:Array[Exp] ) extends Exp  
{   
  def n : Int = a.length
  
  def this( na:Int ) =
      this( new Array[Exp](na) )
  
  def this( args : Exp* ) =
    { this(args.length); var i=0; for( arg<-args ) { a(i) = arg; i=i+1; } }
  
  def this( list : List[Exp] ) =
   { this(list.length); var i=0; for( arg<-list ) { a(i) = arg; i=i+1; } }

  def this( vex:Vex ) = // Copy constructor
    { this(vex.n); for( i <- this ) a(i) = vex(i) }  

  def apply(  i:Int ) = a(i)
  def update( i:Int, b:Exp ) { a(i) = b }

  def + ( v:Vex ) : Vex = 
    { val u = new Vex(n); for( i <- this ) { u(i) = Add(a(i),v(i)) }; u }
  def - ( v:Vex ) : Vex = 
    { val u = new Vex(n); for( i <- this ) { u(i) = Sub(a(i),v(i)) }; u }

  def mul( s:Exp )         : Vex = // Exp * Vex  -- called by Exp
    { val u = new Vex(n); for( i <- this ) { u(i) = Mul(s,a(i)) }; u }

  def * ( v:Vex ) : Vex = // Vex * Vex
    { val u = new Vex(n); for( i <- this ) { u(i) = Mul(a(i),v(i)) }; u }

  override def * ( s:Exp ) : Vex = // Vex * Exp
    { val u = new Vex(n); for( i <- this ) { u(i) = Mul(a(i),s) }; u }

  def * ( b:Mex ) : Vex = // Vex * Mex
  {
    if( n != b.n ) throw new Error()
    val c = new Vex(b.m)
    for( j <- c )
    {
      c(j) = Mul(a(j),b(0,j))
      for( i <- 1 until n )
        c(j) = Add(c(j),Mul(a(j),b(i,j)))
    }
    c
  }

  def dot ( v:Vex ) : Exp = // Vex dot Vex = Exp scalar product
  { 
    val u:Vex = this * v
    var d:Exp = u(0)
    for( i <- 1 until n ) 
      d = Add( d, u(i) )
    d
  }   
           
  def mag  : Exp = 
    { var m:Exp = Num(0); for( i <- this ) { m = Add(m,Mul(a(i),a(i))) }; Sqt(m) }

  def Unit : Vex = 
    { val u = new Vex(n); val s = Rec(mag); for( i <- this ) { u(i) = Mul(a(i),s) }; u; }

  def cos ( b:Vex ) = Div( this dot b, Mul(mag,b.mag) )

// ... override dif, sim ...

  def calcVex( aa:Assign ) : Vec =
  {
    val vec = new Vec(n)
    for( i <- this )
      vec(i) = a(i).calc(aa)
    vec
  }

// ... for comprehensions ...

  def foreach( func:(Int) => Unit )
  {
    var i = 0
    while( i < n )
      { func(i); i+=1 }
  } 

  def map( func:(Exp) => Exp ) : Vex = 
  {
    val vex = new Vex(n)
    for( i <- this )
        vex(i) = func( a(i) )
    vex
  } 

}

object Vex
{  
   val emp : Vex = new Vex(0)

   def apply( exp:Exp ) : Vex = exp match  // This a cast
   {
     case vex:Vex  => vex
     case _        => Logg.trace(4, "Bad Cast", exp.toString); emp
   }   

}

/*
  def cross ( b:Vex ) : Vex = 
  {
    val u = new Vex(n);
    n match
    {
      case 3 => u(0) = a(1)*b(2)-a(2)*b(1);
            u(1) = a(2)*b(0)-a(0)*b(2);
            u(2) = a(0)*b(1)-a(1)*b(0); 
      case _ =>  
    }
    u
   }
*/