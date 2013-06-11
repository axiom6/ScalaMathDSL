package math.ext
import  math.exp._
import  math.util.{ Log=>Logg }

trait Precedence
{
  self:Exp =>
  
  def precedence( op:String ) : Int = op match
  {

    case "="       =>    0
    case "||"      =>    1  // "|" is use for absolute value
    case "&" |"&&" =>    2
    case "=="|"!=" =>    3
    case "<" |"<="|">"|">=" => 4
    case "+" |"-" =>     5
    case "*" |"%" =>     6    
    case "/"      =>     7
    case "^"      =>     8
    case "_"      =>     9
    case "sum"|"itg" => 10
    case "unary"|"unary-"|"unary+"   => 11
    case "d"|"dif"                   => 12
    case "func"                      => 13
    case "cex"|"vex"|"mex"           => 14 // ???
    case "num"|"dbl"|"rat"|"var"     => 15 // ???
    case "("|")"|"{"|"}"|"["|"]"|"|" => 16 // paranthesies very high                 
    case _ => Logg.error( "Unknown", op, "for precedence" ); highest
  }    
  val highest = 15
    
  val equ   = precedence("=")
  val or    = precedence("||")
  val and   = precedence("&&")
  val equalto=precedence("==")
  val lt    = precedence("<")
  val le    = precedence("<=")
  val gt    = precedence(">")
  val ge    = precedence("<=")
  val add   = precedence("+")
  val sub   = precedence("-")
  val mul   = precedence("*")
  val div   = precedence("/")  // -1 for now
  val sus  = precedence("_") // Subscript
  val sup   = precedence("_")
  val pow   = precedence("^")
  val sum   = precedence("sum")
  val neg   = precedence("unary-")
  val differ = precedence("dif")
  val func  = precedence("func")
  val vex   = precedence("vex")
  val num   = precedence("num")
  val par = precedence("(")
  val abs   = precedence("|")
  
  def precedence( exp:Exp ) : Int = exp match
  {
    case Num(n)    => num
    case Dbl(r)    => num
    case Rat(n,d)  => num
    case Var(s)    => num
    case Add(u,v)  => add
    case Sub(u,v)  => sub
    case Mul(u,v)  => mul
    case Div(u,v)  => div
    case Rec(u)    => div
    case Pow(u,v)  => pow
    case Neg(u)    => neg
    case Abs(u)    => abs
    case Par(u)    => par
    case Brc(u)    => par   
    case Lnn(u)    => func
    case Log(u,b)  => func
    case Roo(u,r)  => func
    case Eee(u)    => func
    case Sqt(u)    => func
    case Sin(u)    => func
    case Cos(u)    => func
    case Tan(u)    => func
    case Csc(u)    => func
    case Sec(u)    => func
    case Cot(u)    => func
    case ASin(u)   => func
    case ACos(u)   => func
    case ATan(u)   => func
    case ACsc(u)   => func
    case ASec(u)   => func
    case ACot(u)   => func
    case Equ(u,v)  => equ
    case Dif(u)    => differ
    case Sus(u,v)  => func    
    case Sup(u,v)  => func    
    case Lim(u,v)  => func    
    case Itg(u)    => sum
    case Itl(a,b,u)=> sum
    case Sum(a,b,u)=> sum  
    case Cex(r,i)  => vex
    case Vex(a)    => vex
    case Mex(m)    => vex
    case Not(u)    => func
    case Msg(t)    => func
  }
 
// ADT operators with spacing for the lower precendence
   def operator( exp:Exp ) : String = exp match
   {
     case Equ(u,v)  => " = "
     case Add(u,v)  => "+"
     case Sub(u,v)  => "-"
     case Mul(u,v)  => "*"
     case Div(u,v)  => "/"
     case Pow(u,v)  => "^"
     case Eee(u)    => "^"
     case Neg(u)    => "unary-"
     case Dif(u)    => "d"
     case Itg(u)    => "Int"
     case Itl(a,b,u)=> "Int"
     case Sum(a,b,u)=> "sum"
     case _         => ""
  }
  
}