package algebra

trait Differentiate
{
  this:Exp => // Makes Differentiate aware of Exp for Internal DSL

  def d( e:Exp ) : Exp = e match
  {
    case Num(n)   => 0           // diff of constant zero
    case Var(s)   => Dif(Var(s)) // x becomes dx
    case Par(u)   => Par(d(u))
    case Neg(u)   => -d(u)
    case Pow(u,v) => v * u~^(v-1) * d(u)
    case Mul(u,v) => v * d(u) + u * d(v)
    case Div(u,v) => Par( v*d(u) - u*d(v) ) / v~^2
    case Add(u,v) => d(u) + d(v)
    case Sub(u,v) => d(u) - d(v)
    case Dif(u)   => Dif(d(u))   // 2rd dif
  }
}