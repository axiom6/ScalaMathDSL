
package math.exp
import  math.num.Mat
import  math.util.{  Log=>Logg }

case class Mex( mat:Array[Vex] ) extends Exp
{
  def n:Int = mat.length
  def m:Int = mat(0).n  
   
  def this( nr:Int, mc:Int ) =
      this( Mex.mat(nr,mc) )

  def this( n:Int, v:Exp* ) =
  {
    this( n, v.length/n )
    for( i <- 0 until n )
      for( j <- 0 until m )
        this(i,j) = v(i*m+j)
  }

  def this( ves:List[Exp]  ) =
  {
    this( ves.length, ves.head.asInstanceOf[Vex].n )
    var i = 0
    for( e <- ves )
    {
      val vex = e.asInstanceOf[Vex]
      for( j <- 0 until m )
        { this(i,j) = if( j < vex.n ) vex(j) else Num(0) }
      i = i + 1
    }
  }

  def apply( i:Int, j:Int ) : Exp = mat(i)(j)
  def a(     i:Int, j:Int ) : Exp = mat(i)(j)
  def a : Array[Vex] = mat

  def update( i:Int, j:Int, b:Exp )   { mat(i)(j) = b }

  def copy : Mex = Mex( mat )

  def sub( br:Int, er:Int, bc:Int, ec:Int ) : Mex = 
  {
    val mex:Mex = new Mex( er-br+1, ec-bc+1 )
    for( i <- br until er+1 )
      for( j <- bc until ec+1 )
        mex(i-br,j-bc) = a(i,j)
    mex
  }

  def row( i:Int ) : Vex =
  {
    val d:Array[Exp] = new Array[Exp](m)
    for( j <- 0 until m )
      d(j) = a(i,j)
    new Vex(d)
  }

  def col( j:Int ) : Vex = 
  {
    val d:Array[Exp] = new Array[Exp](n)
    for( i <- 0 until n )
      d(i) = a(i,j)
    new Vex(d)
  }

  def diag : Vex =
  {
    val d:Array[Exp] = new Array[Exp](n)
    for( i <- 0 until n )
      d(i) = a(i,i)
    new Vex(d)
  }

  def + ( b:Mex ) : Mex = 
  {
    val c = new Mex(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )
        c(i,j) = Add(a(i,j),b(i,j))
    c
  }

  def - ( b:Mex ) : Mex =
  {
    val c = new Mex(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )
        c(i,j) = Sub(a(i,j),b(i,j))
    c
  }

  def mul ( b:Exp ) : Mex = /// Exp * Mex -- called by Exp
  {
    val c:Mex = new Mex(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )          
        c(i,j) = Mul(b,a(i,j))
    c
  }

  override def * ( b:Exp ) : Mex = // Mex * Exp
  {
    val c:Mex = new Mex(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )          
        c(i,j) = Mul(a(i,j),b)
    c
  }

  def * ( b:Vex ) : Vex = // Mex * Vex
  {
    val c = new Vex(b.n)
    for( i <- 0 until n )
      for( j <- 0 until m )
        { c(i) = if(j==0) Mul(a(i,j),b(j)) else Add(c(i),Mul(a(i,j),b(j))) }
    c
  }

  def * ( b:Mex ) : Mex = // Mex * Mex
  {
    val c:Mex = new Mex(n,b.m)
    for( i <- 0 until n )
      for( j <- 0 until b.m )
        for( k <- 0 until m )          
          { c(i,j) = if(k==0) Mul(a(i,k),b(k,j)) else Add(c(i,j),Mul(a(i,k),b(k,j))) }
     c
  }

  def transpose : Mex = 
  {
    val mex = new Mex( m, n )
    for( j <- 0 until m )
      for( i <- 0 until n )
        mex(j,i) = a(i,j)
    mex
  }
  
  def calcMex( aa:Assign ) : Mat =
  {
    val ma = new Mat(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )
        ma(i,j) = a(i,j).calc(aa)
    ma
  }  

// ... for comprehensions ...

  def foreach( func:(Exp) => Unit )
  {
    for( i <- 0 until n )
      for( j <- 0 until m )
        func(a(i,j))
  } 

  def map( func:(Exp) => Exp ) : Mex = 
  {
    val mex = new Mex(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )
        mex(i,j) = func( a(i,j) )
    mex
  }  
  
  def zero()
  {
    for( i <- 0 until n )
      for( j <- 0 until m )
        this(i,j) = Num(0)
  }  

// ... experimental ....

    def inv2x2  : Mex =
    {
       val inv  = new Mex( 2, 2 )
       val fac  = Rec( Sub( Mul(a(0,0),a(1,1)), Mul(a(0,1),a(1,0)) ) )
       inv(0,0) =     Mul(fac,a(1,1));  inv(0,1) = Neg(Mul(fac,a(0,1)))
       inv(1,0) = Neg(Mul(fac,a(1,0))); inv(1,1) =     Mul(fac,a(0,0))
       inv
    }
}

object Mex
{
  def mat(nr:Int, mc:Int) : Array[Vex] = 
  {
    val ves = new Array[Vex](nr)
    for( i <- 0 until nr )
      ves(i) = new Vex(mc)
    ves
  }   
  
   val emp : Mex = new Mex(0,0)

   def apply( exp:Exp ) : Mex = exp match  // This a cast
   {
     case mex:Mex  => mex
     case _        => Logg.trace(4, "Bad Cast", exp.toString); emp
   }   

}
