package algebra
import  scala.util.parsing.combinator.lexical.StdLexical
import  scala.util.parsing.combinator.syntactical.StdTokenParsers

object Parse extends StdTokenParsers
{
  type Tokens = StdLexical
  val lexical = new StdLexical
  lexical.delimiters ++= List( "(",")","+","-","^","/","*" )

  def NUM:Parser[Exp] = numericLit        ^^ { (s:String)   => Num(s.toDouble)}
  def VAR:Parser[Exp] = ident             ^^ { (s:String)   => Var(s) }
  def par:Parser[Exp] = "(" ~> end <~ ")" ^^ { (u:Exp)      => Par(u) }
  def neg:Parser[Exp] = "-" ~  end        ^^ { case "-" ~ u => Neg(u)}

  def beg:Parser[Exp] = NUM | VAR | par | neg
  def pow:Parser[Exp] = beg * ( "^" ^^^ { (u:Exp,v:Exp) => Pow(u,v) } )
  def mul:Parser[Exp] = pow * ( "*" ^^^ { (u:Exp,v:Exp) => Mul(u,v) } )
  def div:Parser[Exp] = mul * ( "/" ^^^ { (u:Exp,v:Exp) => Div(u,v) } )
  def add:Parser[Exp] = div * ( "+" ^^^ { (u:Exp,v:Exp) => Add(u,v) } )
  def sub:Parser[Exp] = add * ( "-" ^^^ { (u:Exp,v:Exp) => Sub(u,v) } )
  def dif:Parser[Exp] = "d" ~ sub   ^^  { case "d" ~ u  => Dif(u)   }
  def end:Parser[Exp] = dif | failure("end")

  def apply( str:String ) : Exp = parse( str )
  def parse( str:String ) : Exp = end( new lexical.Scanner(str) ) match
  {
    case Success( exp, _    ) => exp
    case Failure( msg, trc  ) => Var("Failure")
    case Error(   msg, trc  ) => Var("Error")
    case _                    => Var("Unknown")
  }
}
