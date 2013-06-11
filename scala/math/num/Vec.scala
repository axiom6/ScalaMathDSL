
package math.num
import  math.util.Text

class Vec( _n:Int )
{   
    var a   = new Array[Double](_n)
    def n = a.length 

    def this( args : Double* ) = 
      { this( args.length); var i=0; for( arg<-args ) { a(i) = arg; i+=1 } }
    def this( arg:Array[Double] ) = 
      { this( arg.length); for( i <- this ) a(i) = arg(i) }
    def this( arg:Array[Int]    ) = 
      { this( arg.length); for( i <- this ) a(i) = arg(i) }
    def this( arg:List[Double]  ) = 
      { this( arg.length ); for( i <- this ) a(i) = arg(i) }
    def this( arg:Vec ) =          
      { this( arg.n );      for( i <- this ) a(i) = arg(i) }

    def apply(  i:Int )           : Double = a(i)
    def update( i:Int, b:Double ) { a(i) = b }

    def ^ ( a:Double, n:Int ) = Math.pow(a,n) 
    def + ( v:Vec    ) : Vec = 
      { val u = new Vec(n); for( i <- this ) { u(i) = a(i) + v(i) }; u; }
    def - ( v:Vec    ) : Vec = 
      { val u = new Vec(n); for( i <- this ) { u(i) = a(i) - v(i) }; u; }
    def dot ( v:Vec    ) : Double = 
      { var dot:Double = 0.0; for( i <- this ) { dot = dot + a(i) * v(i) }; dot; } // Dot   product
    def *   ( v:Vec    ) : Double = dot(v)
    def *   ( s:Double ) : Vec = map( a => a*s )
   //  { var u = new Vec(n); for( i <- this ) { u(i) = a(i) * s }; return u; }
    def *   ( b:Mat    ) : Vec =
    {
        if( n != b.n ) throw new Error()
        val c = new Vec(b.m)
        for( j <- c )
        {
          c(j) = 0.0
          for( i <- this )
            c(j) = c(j) + a(j) * b(i,j)
        }
        c
    }

    def cos ( b:Vec ) = ( this dot b ) / ( mag * b.mag )

    def cross ( b:Vec ) : Vec = 
    {
        val u = new Vec(n)
        n match
        {
            case 3 => u(0) = a(1)*b(2)-a(2)*b(1)
                      u(1) = a(2)*b(0)-a(0)*b(2)
                      u(2) = a(0)*b(1)-a(1)*b(0)
            case _ =>    
        }
        u
     }
                       
    def mag  : Double = 
      { var m:Double = 0.0; for( i <- this ) { m += a(i)*a(i) }; Math.sqrt(m); }
    def Unit : Vec = 
      { val u = new Vec(n); val s = 1.0/mag; for( i <- this ) { u(i) = a(i)*s }; u; }

    def text : Text =
    { 
        val t:Text = new Text(n*6+4)
        t.app( '[', a(0) )
        for( i <- 1 until n ) 
            t.app( ',', a(i) )
        t.app(']') 
        t
    }
    def str : String = text.str
    override def toString = str

// ... comprehensions ...

// return each i = 0; i < n
   def foreach( f:(Int) => Unit )
   { 
     var i = 0
     while( i < n ) 
       { f(i); i+=1 } 
   }
   
  def map( func:(Double) => Double ) : Vec =
  {
    val vec = new Vec(n)
    for( i <- this )
        vec(i) = func( a(i) )
    vec
  }


}