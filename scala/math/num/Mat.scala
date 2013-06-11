
package math.num
import  math.util.Text
import  Jama._

class Mat private ( _mat:Matrix )
{
  var mat:Matrix = _mat

  def this( n:Int, m:Int )           = this( new Matrix(n,m) )
  def this( a:Array[Array[Double]] ) = this( new Matrix(a) )
  def this( rows:Array[Double]* )    =
  {
    this(Mat.emp.mat)
    val a:Array[Array[Double]] = new Array[Array[Double]](rows.length)
    var i=0; for( row<-rows )
       { a(i) = row; i=i+1; }
    mat = new Matrix(a)
  }

  def this( n:Int, v:Double* ) =
  {
    this(Mat.emp.mat)
    val m = v.length / n
    val a:Array[Array[Double]] = new Array[Array[Double]](n)
    for( i <- 0 until n )
    {
      a(i) = new Array[Double](m)
      for( j <- 0 until m )
        a(i)(j) = v(i*m+j)
    }
    mat = new Matrix(a)
  }

  def n:Int = mat.getRowDimension
  def m:Int = mat.getColumnDimension

  def apply( i:Int, j:Int ) : Double = mat.get(i,j)
  def a(     i:Int, j:Int ) : Double = mat.get(i,j)
  def a : Array[Array[Double]]  = mat.getArray

  def update( i:Int, j:Int, b:Double ) { mat.set(i,j,b) }
  def copy : Mat = new Mat( mat.copy )
  def sub( br:Int, er:Int, bc:Int, ec:Int ) : Mat = 
      new Mat( mat.getMatrix(br,er,bc,ec) )

  def row( i:Int ) : Vec =
  {
    val d:Array[Double] = new Array[Double](m)
    for( j <- 0 until m )
      d(j) = a(i,j)
    new Vec(d)
  }

  def col( j:Int ) : Vec = 
  {
    val d:Array[Double] = new Array[Double](n)
    for( i <- 0 until n )
      d(i) = a(i,j)
    new Vec(d)
  }
  def diag : Vec =
  {
    val d:Array[Double] = new Array[Double](n)
    for( i <- 0 until n )
      d(i) = a(i,i)
    new Vec(d)
  }

  def * ( b:Mat  )   : Mat = new Mat( mat.times(b.mat) )
  def * ( b:Double ) : Mat = new Mat( mat.times(b) )
  def * ( b:Vec  )   : Vec =
  {
    if( m != b.n ) 
      throw new Error()
    val c = new Vec(b.n)
    for( i <- 0 until n )
    {
      c(i) = 0.0
      for( j <- 0 until m )
        c(i) = c(i) + a(i,j) * b(j)
    }
    c
  }

  def + ( b:Mat )  : Mat = new Mat( mat.plus(b.mat) )
  def - ( b:Mat )  : Mat = new Mat( mat.minus(b.mat) )

  def det2x2 : Double = a(0,0)*a(1,1) - a(0,1)*a(1,0)
  def inv2x2 : Mat =
  {
    val inv  = new Mat( 2, 2 )
    val fac  = 1.0 / det2x2
    inv(0,0) =  fac*a(1,1); inv(0,1) = -fac*a(0,1)
    inv(1,0) = -fac*a(1,0); inv(1,1) =  fac*a(0,0)
    inv
  }

  def det3x3 : Double = 
  {
    a(0,0)*a(1,1)*a(2,2) - a(0,0)*a(1,2)*a(2,1) +
    a(0,1)*a(1,2)*a(2,0) - a(0,1)*a(1,0)*a(2,2) +
    a(0,2)*a(1,0)*a(2,1) - a(0,2)*a(1,1)*a(2,0)
  }

  def cof3x3( ii:Int, jj:Int ) : Double =
  {
    var cof:Double = 0.0
    for( i <- 0 until 3 if i!= ii )
      for( j <- 0 until 3 if j!= jj )
        cof += ( a(mod(ii+i),mod(jj+j)) - a(mod(ii+i),mod(jj-j)) )
    cof
  }

  def inv3x3 : Mat =
  {
    val inv  = new Mat( 3, 3 )
    val fac  = 1.0 / det3x3
    for( i <- 0 until 3 )
      for( j <- 1 until 3 )
        inv(i,j) = fac * cof3x3(i,j)
    inv
  }

  def mod( i:Int ) : Int = i%n

  def sign( i:Int, j:Int ) : Double = 
    { if( (i+j)%2 == 0 ) 1.0 else -1.0 }

  def inverse   : Mat = new Mat( mat.inverse   )
  def transpose : Mat = new Mat( mat.transpose )

  def det   : Double = lu.det
  def norm  : Double = mat.norm1
  def trace : Double = mat.trace

  def solve ( b:Mat ) : Mat = new Mat( mat.solve(b.mat) )
 // def solve ( b:Vec ) : Vec = new Vec( mat.solve(b.a) ) 

 // def isSingular : Boolean = mat.isSingular
 // def isSquare   : Boolean = mat.isSquare


  def text           : Text = text( new Text( n*(m*6+4) ) )
  def text( t:Text ) : Text = text( t:Text, "", "" )
  def text( t:Text, sp:String, eol:String ) : Text =
  { 
    val t:Text = new Text( n*(m*6+4) )
    t.app( eol, '[', sp )
    for( i <- 0 until n )
    {
      t.app( eol, '[', sp, a(i,0) )
      for( j <- 1 until m ) 
        t.app( ',', sp, a(i,j) )
      t.app( sp, ']' )
    }
    t.app( eol, sp, ']' )
    t
  }

  def str : String = text.str
  override def toString = str

// ... for comprehensions ...

  def foreach( func:(Double) => Unit )
  {
    for( i <- 0 until n )
      for( j <- 0 until m )
        func(a(i,j))
  } 

  def map( func:(Double) => Double ) : Mat = 
  {
    val mat = new Mat(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )
        mat(i,j) = func( a(i,j) )
    mat
  }  

// ... Jama LU, QR, SV, EV, CH Decompositions ...
  
  private var lt:LUDecomposition = null
  private def lu:LUDecomposition = { if( lt==null ) lt = mat.lu; lt }

  private var qt:QRDecomposition = null
  private def qr:QRDecomposition = { if( qt==null ) qt = mat.qr; qt }

  private var st:SingularValueDecomposition = null
  private def sv:SingularValueDecomposition = { if( st==null ) st = mat.svd; st }

  private var et:EigenvalueDecomposition = null
  private def ev:EigenvalueDecomposition = { if( et==null ) et = mat.eig; et }

  private var ct:CholeskyDecomposition = null
  private def ch:CholeskyDecomposition = { if( ct==null ) ct = mat.chol; ct }

  def lul     : Mat = new Mat( lu.getL )
  def luu     : Mat = new Mat( lu.getU )
  def qrh     : Mat = new Mat( qr.getH )
  def qrq     : Mat = new Mat( qr.getQ )
  def qrr     : Mat = new Mat( qr.getR )
  def svs     : Mat = new Mat( sv.getS )
  def svu     : Mat = new Mat( sv.getU )
  def svv     : Mat = new Mat( sv.getV )
  def eigVec  : Mat = new Mat( ev.getV )
  def eigDia  : Mat = new Mat( ev.getD )
  def eigValR : Vec = new Vec( ev.getRealEigenvalues )
  def eigValI : Vec = new Vec( ev.getImagEigenvalues )
  def chl     : Mat = new Mat( ch.getL )
}

object Mat
{

  def matrix( rows:Array[Double]* ) : Matrix =
  {
    val a:Array[Array[Double]] = new Array[Array[Double]](rows.length)
    var i=0
    for( row<-rows )
       { a(i) = row; i=i+1; }
    new Matrix(a)
  }

  val emp : Mat = new Mat(1,1)
}