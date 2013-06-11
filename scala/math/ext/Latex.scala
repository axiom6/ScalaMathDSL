package math.ext
import  math.exp._
import  math.util.Text

// Not implemented yet

trait Latex
{
   self:Exp =>
     
   def latex(  t:Text ) {  latex(t,this) }

   def latex( t:Text, exp:Exp )
   {
     exp match
     {
      case Num(n)    => t.app( n.toString )
      case Dbl(r)    => t.app( r.toString )
      case Rat(n,d)  => t.app( n.toString, '/', d.toString )
      case Var(s)    => t.app( s ) // t.app( Syms.sym(s) )
      case Add(u,v)  => u.latex(t); t.app('+'); v.latex(t)
      case Sub(u,v)  => u.latex(t); t.app('-'); v.latex(t)
      case Mul(u,v)  => group(t,u); t.app('*'); group(t,v)
      case Div(u,v)  => group(t,u); t.app('/'); denom(t,v)
      case Rec(u)  => t.app("1"); t.app('/'); group(t,u)
      case Pow(u,v)  => group(t,u); t.app('^'); group(t,v)
      case Neg(u)    => t.app('-'); u.latex(t)
      case Abs(u)    => t.app('|'); u.latex(t); t.app('|')
      case Par(u)    => paren(t,u)
      case Brc(u)    => t.app('{'); u.latex(t); t.app('}')
      case Lnn(u)    => latex( t, "ln", u )
      case Log(u,b)  => latex( t, "log", b.r, u )
      case Roo(u,r)  => latex( t, "root",r.r, u )
      case Eee(u)    => t.app("e^"); group(t,u)
      case Sqt(u)    => latex( t, "sqrt",   u )
      case Sin(u)    => latex( t, "sin",    u )
      case Cos(u)    => latex( t, "cos",    u )
      case Tan(u)    => latex( t, "tan",    u )
      case Csc(u)    => latex( t, "csc",    u )
      case Sec(u)    => latex( t, "sec",    u )
      case Cot(u)    => latex( t, "cot",    u )
      case ASin(u)   => latex( t, "arcsin", u )
      case ACos(u)   => latex( t, "arccos", u )
      case ATan(u)   => latex( t, "arctan", u )
      case ACsc(u)   => latex( t, "arccsc", u )
      case ASec(u)   => latex( t, "arcsec", u )
      case ACot(u)   => latex( t, "arccot", u )
      case Equ(u,v)  => u.latex(t); t.app('='); v.latex(t)
      case Dif(u)    => latexDif(t,u)
      case Sus(u,v)  => u.latex(t); t.app('_'); v.latex(t)
      case Sup(u,v)  => u.latex(t); t.app('^'); v.latex(t)
      case Lim(u,v)  => t.app('_'); u.latex(t); t.app('^'); v.latex(t)
      case Itg(u)    => latex( t, "Int", u )
      case Itl(a,b,u)=> latex( t, "Int", a, b, u )
      case Sum(a,b,u)=> latex( t, "sum", a, b, u )
      case Cex(r,i)  => latexCex(t,r,i)
      case Vex(a)    => latexVex(t,a)
      case Mex(mat)  => latexMex(t,mat)
      case Msg(txt:Text) => t.app(txt)
    }
   }
  
// Function
   def latex( t:Text, func:String, u:Exp )
     { t.app(func); paren(t,u) }

// Function subscript
   def latex( t:Text, func:String, r:Double, u:Exp )
     { t.app(func,'_',r); paren(t,u) }
   
// Function subscript superscript
   def latex( t:Text, func:String, a:Exp, b:Exp, u:Exp )
    { t.app(func,'_'); a.latex(t); t.app('^'); b.latex(t); paren(t,u) } 
  
   def latexDif( t:Text, u:Exp )
   {
     u match
     {
       case Var(s) =>
         if( s.length==1 )
           t.app( 'd', s )
         else
           t.app( "d(", s, ')' )
        case _ =>
           ascii( t, "d", u )
     }
   }
  
   def latexCex( t:Text, r:Exp, i:Exp )
    { t.app('['); latex(t,r); t.app(','); latex(t,i); t.app("\\ii]") } 
  
   def latexVex( t:Text, a:Array[Exp] )
  { 
    t.app( "\\begin{bmatrix}" ); a(0).latex(t)
    for( i <- 1 until a.length ) 
      { t.app(" & "); a(i).latex(t) }
    t.app( "\\end{bmatrix}" )
  }
   
   def latexMex( t:Text, mat:Array[Vex] )
  { 
    t.app( "\\begin{bmatrix}" )
    for( i <- 0 until mat.length )
    { 
      mat(i)(0).latex(t)
      for( j <- 1 until mat(i).n )
        { t.app(" & "); mat(i)(j).latex(t) }
      if( i < mat.length-1 ) t.app("\\\\")
    }
    t.app( "\\end{bmatrix}" )
  }
   
}