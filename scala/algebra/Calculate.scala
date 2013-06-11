package algebra

trait Calculate
{
  this:Exp => // this is Exp
  val NaN : Double = Double.NaN
  type Assign = (String) => Double // { “a”=>3.0, “b”=>6.0 }

  def calc( e:Exp, a:Assign ) : Double = e match
  {
    case Num(d)   => d          // Unwrap the double
    case Var(s)   => a(s)       // Return double assigned to variable
    case Par(u)   => calc(u,a)  // Strip off parentheses
    case Neg(u)   => -calc(u,a)
    case Pow(u,v) => Math.pow( calc(u,a), calc(v,a) )
    case Mul(u,v) => calc(u,a) * calc(v,a)
    case Div(u,v) => val de=calc(v,a); if(de==0.0) NaN else calc(u,a)/de
    case Add(u,v) => calc(u,a) + calc(v,a)
    case Sub(u,v) => calc(u,a) - calc(v,a)
    case Dif(u)   => NaN
  }
}
