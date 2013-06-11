
package math.util


object Log
{
  val  text:Text = new Text(1000)
  type CS        = Text.CS

// ... log and put output
// log is the most called method that clears the buffer, delimits args and
//   outputs a newline
// put is more generic in that does not delimit the args and does not
//  output a newline so put can be called multiple times to log a line of text

   def log( args:Any* ) { text.clear(); arg(args); eol() }
   def put( args:Any* ) { text.clear(); seq(args); out() }

// .. Format args ...

  def app(   args:Any*     ) { text.seq(args)  }
  def seq(   args:Seq[Any] ) { text.seq(args)  }
  def delim( args:Seq[Any] ) { text.delim(" ",args)  }

// ... log error trace stack output ...

// Delimit with spaces and output args
   def arg( args:Seq[Any] ) { text.delim(" ",args); out() }

// ... error trace stack ...
  def error(        args:Any* ) { msg("Error:  "); arg(args); tracen(1); eol() }
  def error( n:Int, args:Any* ) { msg("Error:  "); arg(args); tracen(n); eol() }
  def trace( n:Int, args:Any* ) { msg("Trace:  "); arg(args); tracen(n); eol() }

  def except( e:Exception, args:Any* )
  {
    msg("Except: "); arg(args)
    if( e != null )
      { app( " (", e.getMessage, ") " ); out(); tracen(4) }
    eol()
  }

  def stack( e:Exception, args:Any* )
  {
    msg("Stack:  "); arg(args)
    if( e != null )
        e.printStackTrace()
    eol()
  }

  def fatal( args:Any* )
    { msg("Fatal:  "); arg(args); tracen(6); eol() /*; Io.exit() */ }

// ... tracen ...

  private def tracen( n:Int ) { tracen( n, new Exception ) }

  private def tracen( n:Int, except:Exception )
  {
    text.clear()
    val stack  = except.getStackTrace
    var name   = "Unknown"

    app(" [")
    for( i <- 2 until Math.min(n+2,stack.length) )
    {
      name = stack(i).getFileName
      name = name.substring( 0, name.indexOf('.') )
      app( name, '.', stack(i).getMethodName, ':', stack(i).getLineNumber, ' '  )

    }
    text(text.len-1) = ']'
    out()
  }

// ... tab array ...

  def array( tag:CS, n:Int, arr:Array[Char] )
  {
     text.app( tag, ' ', n, " [ " )
     for( a <- arr )
       text.app( a, ' ' )
     if( text.len > 1 )
         text.len -= 1
     text.app( " ]" )
     out()
     eol()
  }

  def tab( n:Int, args:Any* )
  {
     text.tab(n)
     text.seq(args)
     out()
     eol()
  }

// ... Console Output ...

  def out()
  {
    for( i <- 0 until text.len )
      Console.out.print( text(i) )
    Console.out.flush()
    text.clear()
  }

// ... MessageConsole Output ...
   def msg( cs:CS )
   {
     for( i <- 0 until cs.length )
       Console.out.print( cs.charAt(i) )
     Console.out.flush()
   }

  def eol()   { msg( Text.eol ) }
  def tab()   { msg( Text.tab ) }
  def space() { msg( " "      ) }

}