package math.util


object Text
{
  type CS = CharSequence

  // Constructors
  def apply( cap:Int ) : Text = new Text(cap)
  def apply( cs:CS   ) : Text = new Text(cs)  // Works for Text String etc
  def apply( sq:CS*  ) : Text =
  {
    var len = 0
    for( s <- sq )
      len += s.length
    val text = new Text(len)
    for( s <- sq )
      text.app(s)
    text
  }

  // Constants
  val tab   : String = "  "
  val eol   : String = "\n"
  val delim : String = ","

  // Empty and type checks
  val empty : Text = new Text(0)
  def is( text:Text  ) : Boolean = text!=null && text!=empty
  def isCS( cs:CS ) : Boolean = cs!=null && cs.length > 0

  def toUpper( c:Char ) : Char = java.lang.Character.toUpperCase(c)
  def toLower( c:Char ) : Char = java.lang.Character.toLowerCase(c)


  def equ( a:CS, b:CS )                 : Boolean = Text.equ( a,  0, a.length, b, 0, b.length )
  def equ( a:CS, ab:Int, al:Int, b:CS ) : Boolean = Text.equ( a, ab, al,       b, 0, b.length )

  def equ( a:CS, ab:Int, al:Int,
           b:CS, bb:Int, bl:Int ) : Boolean =
  {
    if( a != null && b != null && al == bl )
    {
      var i   = 0
      while( i < al )
      {
        if( a.charAt(ab+i) != b.charAt(bb+i) )
          return false
        i += 1
      }
      return true
    }
    false
  }

  def equ( a:Array[Char], ab:Int, al:Int,
           b:CS,          bb:Int, bl:Int ) : Boolean =
  {
    if( a != null && b != null && al == bl )
    {
      var i   = 0
      while( i < al )
      {
        if( a(ab+i) != b.charAt(bb+i) )
          return false
        i += 1
      }
      return true
    }
    false
  }

}

class Text( _cap:Int ) extends Text.CS
{
  var a     : Array[Char] = new Array[Char](_cap)
  var len   : Int         = 0

  type CS   = Text.CS


  def this() = this( 100 )
  def this( cs:Text.CS ) =
  {
    this( cs.length )
    len = cs.length
    for( i <- this )
      a(i) = cs.charAt(i)
  }

  // ... apply ...
  def lock()   {}
  def unlock() {}

  def apply( i:Int ) : Char = a(i)

  def foreach( f:Int => Unit )
  {
    var i = 0
    while( i < len )
    { f(i); i +=1 }
  }

  // .. Equals ...

  //def isComparable( any:Any ) : Boolean = any.isInstanceOf[CS]
  override def hashCode : Int = a.hashCode * 41
  /*
  override def equals( any:Any ): Boolean = any match
  {
    case cs:CS   => To.equ(this,cs)
    case _       => false
  }
  */
  // ... String ...

  var string:String = null
  def str:String =
  {
    if( string==null || !equals(string) )
      string = new String(a,0,len)
    string
  }
  override def toString : String = str

  def in( b:Int, e:Int ) : Boolean = in(b)&& in(e-1) && b <= e
  def in( i:Int )        : Boolean = 0 <= i && i < len



  // ... CS CharSequence Interface

  def length : Int   = len
  def charAt( i:Int) : Char = a(i)
  def subSequence( b:Int, e:Int ) : CS =
  { if( in(b,e)) new String(a,b,e-b) else new String("") }

  // ... capacity ...

  def cap             : Int  = a.length
  def cap( _cap:Int )
  {
    if( _cap > cap )
    {
      lock()
      try
      {
        val b : Array[Char] = new Array[Char](_cap+10)
        for( i <- this )
          b(i) = a(i)
        a = b
      }
      finally unlock() 
    }
  }
  def grow( inc:Int )  { cap( len + inc ) }


  // ... Parse Utilities ...

  def has( c:Char ) : Boolean =
  { for( i <- this ) { if( this(i)==c ) return true }; false }

//  def has( cs:CS ) : Boolean = find(cs,0,len,-1) > - 1

  def hasTail( c:Char ) : Boolean = a(len-1)==c

  // --- Mutable Methods ---

  def update( i:Int, c:Char )
  { lock(); try { a(i) = c } finally unlock() }

  def clear()
  { lock(); try { len = 0 } finally unlock() }

  def x : Text =
  { lock(); try { len = 0 } finally unlock(); this }

  def delTail()       { lock(); try { if(len>0) len-=1 } finally unlock() }
  def delTail(c:Char) { lock(); try { if(len>0&&hasTail(c)) len-=1 } finally unlock() }

  // ... app ....

  def app( cs:CS )
  {
    lock()
    try
    {
      grow( cs.length )
      for( j <- 0 until cs.length )
        a(len+j) = cs.charAt(j)
      len += cs.length
    }
    finally unlock() 
  }

  def app( c:Char  )
  {
    lock()
    try
    {
      grow(1)
      a(len) = c
      len += 1
    }
    finally unlock() 
  }

  def app( b:Boolean )
  {
    if(b) app("true") else app("false")
  }

  def app( n:Byte  ) { app( n.toLong   ) }
  def app( n:Short ) { app( n.toLong   ) }
  def app( n:Int   ) { app( n.toLong   ) }
  def app( n:Long  ) { app( n.toString ) }


  def app( d:Float  ) { app(d.toDouble)  }
  def app( d:Double ) { app(d,dec(d))    }

  def dec( dbl:Double ) : Int  =
  {
    var d:Int      = 0
    var r:Double   = rem(dbl)
    if( r < 0.000000000001 )
      return d

    var d1:Int  = 0
    var d2:Int  = 0
    var d3:Int  = 0
    // Console.prIntln( "dbl", dbl, "---------" )
    for( i <- 0 until 15 )
    {                       // Console.prIntln( "r ", r  )
      d1 = dig(r,10);       // Console.prIntln( "d1", d1 )
      d2 = dig(r,100);      // Console.prIntln( "d2", d2 )
      d3 = dig(r,1000);     // Console.prIntln( "d3", d3 )
      if( d1==d2&&d2==d3 )
        return d
      else
        d = d + 1
      r = r * 10
    }
    d
  }

  def rem( dbl:Double ) : Double = dbl - Math.floor(dbl)
  def dig( rem:Double, mul:Int    ) : Int = (rem * mul).toInt % 10

  //  dek = -Math.round(To.log10(rem)).toInt



  def app( u:Unit ) {}
//def app( t:Tok  ) { if(t.ok)    app( To.as[CS](t) ) }
  def app( t:Text ) { if(t!=this) app( t.a ) }

  //def app( b:Boun ) { app("Boun: x=", b.x, " y=", b.y, " w=", b.w, " h=", b.h ) }

  def app( array:Array[Char]  ) { app( array, 0, array.length ) }
/*
  def app( array:Array[Char], b:Int, n:Int )
  {
    lock
    try
    {
      var  m = To.min(n,array.length-b)
      grow(m)
      for( j <- 0 until m )
        a(len+j) = array(b+j)
      len += m
    }
    finally unlock() 
  }
*/
  def any( v:Any )
  {
    lock()
    try
    {
      v match
      {
      //case t:Tok          => app(t)
        case t:Text         => app(t)
        case s:CS           => app(s)
        case c:Char         => app(c)
        case b:Boolean      => app(b)
        case y:Byte         => app(y)
        case i:Int          => app(i)
        case j:Short        => app(j)
        case l:Long         => app(l)
        case f:Float        => app(f)
        case d:Double       => app(d)
        case a:Array[Char]  => app(a)
        case a:Array[Any]   => delim( ",", a )
        case a:List[_]      => delim( ",", a )
      //case e:Enum         => app(e.name)
        case u:Unit         => app(u)
      //case r:Boun         => app(r)
      //case u:Uri          => app(u.text)
        case _              => app(v.toString)
      }
    }
    finally unlock() 
  }

  // ... args:Any* appends - app seq text ...

  def app( args:Any* )
  { lock(); try { for( arg<-args ) any(arg) } finally unlock() }

  def seq( args:Seq[Any] )
  { lock(); try { for( arg<-args ) any(arg) } finally unlock() }

  def delim( mid:CS, args:Seq[Any] )
  { delim( "", mid, "", args ) }

  def delim( beg:CS, mid:CS, end:CS, args:Seq[Any] )
  {
    lock()
    try
    {
      app( beg )
      for( i <- 0 until args.length )
      {
        if(  i < args.length-1 )
          app( args(i), mid )
        else
          app( args(i) )
      }
      app( end )
    }
    finally unlock() 
  }

  def text( args:Any* ) : Text = { seq(args); this }

  // ... delimited args:Any* appends - app seq text ...

  // ... Find matching strings ...

  def find( cs:CS )                       : Int = find( cs, 0, len, -1 )
  def find( cs:CS, b:Int )                : Int = find( cs, b, len, -1 )
  def find( cs:CS, b:Int, e:Int )         : Int = find( cs, b, e,   -1 )
  def find( cs:CS, b:Int, e:Int, df:Int ) : Int = fwd(  cs, b, e,   df )

  // ... Traverse forward ...
  def fwd( cs:CS,_b:Int,_e:Int, df:Int ) : Int =
  {
    var i = Math.max(_b,0)
    val e = Math.min(_e,len)
    while( i < e )
    { if( Text.equ(a,i,e,cs)) return i else i+=1 }
    df
  }


  // ... att css tab ...

  def att( name:CS, value:Any )
  { lock(); try { app( " ", name, "=\"", value, "\"" ); replace('_','-') } finally unlock() }

  def css( name:CS, value:Any )
  { lock(); try { app( " ", name, ": ", value, "; " ); replace('_','-') } finally unlock() }

  def css( name:CS, value:Any, quotes:Boolean )
  { lock(); try { app( " ", name, ": \"", value, "\"; " ); replace('_','-') } finally unlock() }

  def css( name:CS, value:Any, uom:String )
  { lock(); try { app( " ", name, ": ", value, uom, "; " ); replace('_','-') } finally unlock() }

  // ... space tab ...

  def space( n:Int )
  { lock(); try { for( i <-0 until n ) app(' '); } finally unlock() }

  def tab( n:Int )
  { lock(); try { var i = 1; while( i <= n ) { app(Text.tab); i +=1 } } finally unlock() }

  def tab( n:Int, args:Any* )
  { lock(); try { tab(n); seq(args) } finally unlock() }

  // ... del shift ...

  def del( p:Int )        { del( p, 1 ) }
  def del( p:Int, n:Int )
  {
    lock()
    try
    {
      val ns = len - p - n
      for( i <-0 until ns )
        a(p+i) = a(p+n+i)
      len-=n
    }
    finally unlock() 
  }

  def shift( p:Int, n:Int )
  {
    if( n < 0 )
      del( p, -n )
    else if( n > 0 )
    {
      lock()
      try
      {
        grow(n)
        var i = len - 1
        while( i >= p )
        { a(i+n) = a(i); i-=1 }
        len+=n
      }
      finally unlock() 
    }
  }

  // ... replace ...

  def replace( p:Int, n:Int, cs:CS )
  {
    lock()
    try
    {
      shift( p, cs.length-n )
      set(   p, cs )
    }
    finally unlock() 
  }
  /*
  def replace( cso:CS, csn:CS )
  {
    lock()
    try
    {
      var b = 0
      while( b < len )
      {
        b = find( cso, b )
        if( b == -1 )
          return
        replace( b, cso.length, csn )
        b = b + csn.length
      }
    }
    finally unlock() 
  }
  */
  def replace( o:Char, n:Char )
  {
    lock()
    try
    {
      for( i <- this )
      { if( a(i)==o ) a(i)=n }
    }
    finally unlock() 
  }

  // ... add ins set (Char) and ((Any) ...
  def add( p:Int, ch:Char )
  {
    lock(); try
  {
    shift( p, 1 )
    a(p) = ch
  }
  finally unlock() 
  }

  def add( p:Int, cs:CS )
  {
    lock(); try
  {
    shift( p, cs.length )
    set(   p, cs )
  }
  finally unlock() 
  }

  def ins( p:Int, ch:Char ) { add( if(p>0) p-1 else 0, ch ) }
  def ins( p:Int, cs:CS   ) { add( if(p>0) p-1 else 0, cs ) }

  def set( p:Int, ch:Char )
  {
    lock(); try
  {
    a(p) = ch
  }
  finally unlock() 
  }

  def set( p:Int, cs:CS )
  {
    lock(); try
  {
    cap(p+cs.length)
    var j = 0
    while( j < cs.length )
    { a(p+j) = cs.charAt(j); j+=1 }
  }
  finally unlock() 
  }



  // ... Compare Functions












  def text( tx:Text ) : Text = { tx.app(this); tx }

  // ... in ...



  // ... Upper and Lower Case
  def upper( i:Int ) { a(i) = Text.toUpper(a(i)) }
  def lower( i:Int ) { a(i) = Text.toLower(a(i)) }
  def upper()        { for( i <- this ) upper(i) }
  def lower()        { for( i <- this ) lower(i) }



  def sub( o:Int, n:Int ) : CS = subSequence( o, o+n )



} // End of class Text


