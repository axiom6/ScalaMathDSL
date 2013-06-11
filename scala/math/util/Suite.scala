
package math.util

import  math.exp._
import  math.num._
import  math.parse._


object Suite
{
   val tst = new Suite
   def test()
   {       
       tst.testExp()
       tst.testCex()
       tst.testVex()
       tst.testMex()
       tst.testRun()
       tst.testEee()
       tst.testDif()
       tst.testItg()
       tst.testFun()
       tst.testSum()
       tst.testEqu()
       tst.testSim()
    // tst.testSus
    // tst.testErr
    // tst.testFail       
   } 
}
  
class Suite //extends Suite
{
   type CS     = Text.CS
   type Assign = String => Double

  def pars( name:CS, txt:Text, seq:String* )
  {
    txt.clear()
    for( str <- seq )
      txt.app( str, ' ' )
    Test.keep( name, txt )
    txt.clear()
    for( str <- seq )
    {
      AsciiParse(str).ascii(txt)
      txt.app(' ')
    }
    Test.test( name, txt )
  }
  
  def par( name:CS, txt:Text, str:String )
  {
    txt.clear()
    Test.keep( name, str )
      AsciiParse(str).ascii(txt)
    Test.test( name, txt )  
  }

  def sim( name:CS, txt:Text, enter:String, bench:String )
  {
    txt.clear()
    Test.keep( name, bench )
      AsciiParse(enter).sim.ascii(txt)
    Test.test( name, txt )  
  }  

  def lam( name:CS, txt:Text, str:String, lam:String )
  {
    txt.clear()
    Test.keep( name, str, ' ', lam )
      val exp:Exp = AsciiParse(str)
      exp.ascii(txt)
      txt.app( ' ' )
      exp.lambda(txt)
    Test.test( name, txt )  
  }  
  
  def itg( name:CS, txt:Text, es:String, is:String )
  {
    txt.clear()    
    Test.keep( name, es, "::", is )
      val exp = AsciiParse(es)
          exp.ascii(txt)
          txt.app( "::" )
      val Ixp = exp.itg.sim
          Ixp.ascii(txt)
    Test.test( name, txt )  
  }  

   def testExp()
   {
    val t = new Text(200)

    Test.keep( "lam.a", "x+x+7+y" )
      val exp:Exp = Add(Add(Var("x"),Var("x")),Add(Num(7),Var("y")))
      exp.ascii(t.x)
    Test.test( "lam.a", t )

    val env:Assign = { case "x" => 5 case "y" => 7 }
    Test.keep( "calc.a", "<x=5 y=7> ", t, " = ", 24 )
    Test.test( "calc.a", "<x=5 y=7> ", t, " = ", exp.calc(env) ) 
    
    Test.keep( "pow.a", "(x+y)^3" )
      val pow:Exp = Pow(Add(Var("x"),Var("y")),Num(3))
      pow.ascii(t.x)
    Test.test( "pow.a", t )    
    
    val ppp:Assign = { case "x" => 2 case "y" => 1 }
    Test.keep( "calc.b", "<x=2 y=1> ", t, " = ", 27 )
    Test.test( "calc.b", "<x=2 y=1> ", t, " = ", pow.calc(ppp) )     

    Test.keep( "pow.dif.x", "3*(x+y)^2*(dx+dy)" )
      pow.dif.ascii(t.x)
    Test.test( "pow.dif.x", t )      
   }
 
   def testCex()
   {
     val t = new Text(200)
     lam( "cex.a", t, "[a,b.i]",       "Cex(Var(a),Var(b))" )
     lam( "cex.b", t, "[a+b,(c+d).i]", "Cex(Add(Var(a),Var(b)),Par(Add(Var(c),Var(d))))" )
     lam( "cex.c", t, "i*a*i", "Mul(Mul(Var(i),Var(a)),Var(i))" )
   }
   
   def testVex()
   {
     val t = new Text(200)
     
     par( "vex.a", t, "[a,b]" )
     par( "vex.b", t, "[x^2,y^3,z^4]" ) 
    
   }   

   def testMex()
   {
     val t = new Text(200)
     
     par( "mex.parse.ma", t, "[[x,x^2,x^3][y,y^2,y^3][z,z^2,z^3]]" )
     
     val sb = "[[dx,2*x*dx,3*x^2*dx][dy,2*y*dy,3*y^2*dy][dz,2*z*dz,3*z^2*dz]]"
     Test.keep( "mex.dif.mb", sb )
       val eb:Exp = AsciiParse( sb )
       val mb:Mex = Mex(eb)
           mb.dif.sim
           mb.ascii(t.x)
     Test.test( "mex.dif.mb", t )  
   
     val rc:Assign = { case "x"=>1 case "y"=>2 case "z"=>3 }
     val nc = "[[1,1,1][2,4,8][3,9,27]]"
     Test.keep( "mex.eval.mc", nc ) 
       val ea:Exp = AsciiParse("[[x,x^2,x^3][y,y^2,y^3][z,z^2,z^3]]")
       val ma:Mex = Mex(ea)
       val mc:Mat = ma.calcMex(rc)
     Test.test( "mex.eval.mc", mc.text(t.x) ) 

     par( "mex.d.var.a", t, "[[a,b,c][d,e,f][g,h,i]]" )    
     par( "mex.d.var.b", t, "[[a,b^2][c,d^2]]" ) 
     
     val sf = "[[a,b][c,d]]"
     Test.keep( "mex.f.inv2x2.a", "[[1/(a*d-b*c)*d,-1/(a*d-b*c)*b][-1/(a*d-b*c)*c,1/(a*d-b*c)*a]]" ) 
       val ef:Exp = AsciiParse( sf )
       val mf:Mex = Mex(ef)
       val fm:Exp = mf.inv2x2
         fm.ascii(t.x)       
     Test.test( "mex.f.inv2x2.a", t ) 
     
     val sg = "[[x,x^2,x^3][y,y^2][z]]"
     Test.keep( "mex.g", "[[x,x^2,x^3][y,y^2,0][z,0,0]]" ) 
     val mg:Exp = AsciiParse( sg )
         mg.ascii(t.x)
     Test.test( "mex.g", t )    
   
     par(  "mex.h", t, "[[x^2,y^3,z^4][x^2,y^3,z^4][x^2,y^3,z^4]]" ) 
    
   }

 def testRun()
  {
    val t = new Text(200)
    // -------------- x ------------------" )
    pars( "x.y.a...", t, "5", "dx", "x^y", "x*y", "x/y", "x+y", "x-y", "(x+y)", "-x" )
    pars( "tran.fun", t, "exp^x", "ln(x)", "sqrt(x)", "log_10(x)", "log_2(x)" )
    pars( "trig.sin", t, "sin(x)", "cos(x)", "tan(x)", "cot(x)", "sec(x)", "csc(x)" )
 // pars( "trig.ltx", t, "\\sin(x)", "\\cos(x)", "\\tan(x)", "\\cot(x)", "\\sec(x)", "\\csc(x)" )
    pars( "trig.arc", t, "arcsin(x)", "arccos(x)", "arctan(x)", "arccot(x)", "arcsec(x)", "arccsc(x)" )
    // -------------- ops ------------------" )
    pars( "x,y,z.a", t, "x*y*z", "x*y/z", "x*y+z", "x*y-z"  )    
    pars( "x,y,z.b", t, "x*y*z", "x/y*x", "x+y*x", "x-y*x" )
    // -------------- par ------------------" )
    pars( "paren.a", t, "x*(y+z)", "x/(y+z)", "x+(y+z)", "x-(y+z)" )
    pars( "paren.b", t, "x*(y*z)", "x*(y/z)", "x*(y+z)", "x*(y-z)" )    
    pars( "paren.c", t, "(x*y)*z", "(x/y)*x", "(x+y)*x", "(x-y)*x" )
    // -------------- par op ------------------" )
    pars( "paren.d", t,  "w*(x+y)*z",   "x/(y+z)/x",   "x+(y*z)+x",   "x-(y*z)-x"  )
    pars( "paren.e", t, "(x*(y+z))*x", "(x/(y+z))/x", "(x+(y*z))+x", "(x-(y*z))-x" )
    pars( "paren.f", t, "x*((y+z)*x)", "x/((y+z)/x)", "x+((y*z)+x)", "x-((y*z)-x)" )
    // -------------- pow ------------------" )
    pars( "power.a", t, "(x+y)^3", "ln(x^2)", "ln(x^2)", "log_2(x^3)", "ln(x^2)", "sqrt(x^4)" )  
    // -------------- secondary ------------------" )
    pars( "exp.d.a", t, "exp^x", "-x", "dx", "exp^(x+y)", "(x+y)*dx", "-(x+y)"  )
    // -------------- vex mex ------------------" )
    pars( "vex...a", t,  "[x,x^2,x^3]" )
    pars( "a+mex.a", t, "a+[[x,x^2,x^3][y,y^2,y^3][z,z^2,z^3]]" ) 
    // -------------- Root ------------------" )
    pars( "root..a", t, "root_3(x)", "root_3(x+2)", "sqrt(x+2)" ) // "sqrt_3(x+2)" "sqrt[3](x+2)" 
    
    lam( "Lam.sin.a", t, "sin(x)", "Sin(Var(x))" )
    
  } 

   def testEee()
   {
     val t = new Text(200)
    lam(  "eee.a", t, "e^x", "Eee(Var(x))")
    lam(  "eee.b", t, "e^(x+2)", "Eee(Par(Add(Var(x),Num(2))))")
    lam(  "eee.c", t, "e", "Var(e)")
    lam(  "eee.d", t, "ln(e)",   "Lnn(Var(e))")
    lam(  "eee.e", t, "ln(e^1)", "Lnn(Eee(Num(1)))")
    lam(  "eee.f", t, "e*x", "Mul(Var(e),Var(x))" )
    lam(  "eee.g", t, "a-e", "Sub(Var(a),Var(e))" )
    lam(  "eee.h", t, "e^e^x", "Eee(Eee(Var(x)))" )
    lam(  "eee.i", t, "d+e", "Add(Var(d),Var(e))")
   }
  
   def testDif()
   {
     val t = new Text(200)
     
     val sa = "x*y*z"
     Test.keep( "dif.a", "z*y*dx+z*x*dy+x*y*dz" )
       val ea:Exp = AsciiParse( sa )
       val da:Exp = ea.dif.sim
           da.ascii(t.x)
     Test.test( "dif.a", t )
     
      val sb = "x^2*y^3*z^4"
     lam( "lam.sb", t, sb, "Mul(Mul(Pow(Var(x),Num(2)),Pow(Var(y),Num(3))),Pow(Var(z),Num(4)))")
     Test.keep( "dif.sb", "z^4*y^3*2*x*dx+z^4*x^2*3*y^2*dy+x^2*y^3*4*z^3*dz" )
       val eb:Exp = AsciiParse( sb )
       val db:Exp = eb.dif.sim
           db.ascii(t.x)
     Test.test( "dif.sb", t )   
   
     val sc = "w*x^2*y^3*z^4"
     Test.keep( "dif.c", "z^4*y^3*x^2*dw+z^4*y^3*w*2*x*dx+z^4*w*x^2*3*y^2*dy+w*x^2*y^3*4*z^3*dz" )
       val ec:Exp = AsciiParse( sc )
       val dc:Exp = ec.dif.sim
           dc.ascii(t.x)
     Test.test( "dif.c", t )       
   
     val sd = "sin(x)+cos(x)+tan(x)"
     Test.keep( "dif.d", "cos(x)*dx-sin(x)*dx-sec(x)^2*dx" )
       val ed:Exp = AsciiParse( sd )
       val dd:Exp = ed.dif.sim
           dd.ascii(t.x)
     Test.test( "dif.d", t ) 
   
     val se = "csc(x)+sec(x)+cot(x)"
     Test.keep( "dif.e", "-csc(x)*cot(x)*dx+sec(x)*tan(x)*dx-csc(x)^2*dx" )
       val ee:Exp = AsciiParse( se )
       val de:Exp = ee.dif.sim
           de.ascii(t.x)
     Test.test( "dif.e", t ) 
   
      val sf = "arcsin(x)+arccos(x)+arctan(x)"
     Test.keep( "dif.f", "dx/sqrt(1-x^2)-dx/sqrt(1-x^2)+dx/(1+x^2)" )
       val ef:Exp = AsciiParse( sf )
       val df:Exp = ef.dif.sim
           df.ascii(t.x)
     Test.test( "dif.f", t ) 
   
     val sg = "arccsc(x)+arcsec(x)+arccot(x)"
     Test.keep( "dif.g", "-dx/(x*sqrt(x^2-1))+dx/(x*sqrt(x^2-1))-dx/(1+x^2)" )
       val eg:Exp = AsciiParse( sg )
       val dg:Exp = eg.dif.sim
           dg.ascii(t.x)
     Test.test( "dif.g", t ) 
   
     lam( "dx.a",     t, "dx", "Dif(Var(x))" ) 
     lam( "Var(d).a", t, "d",  "Var(d)" ) 
     lam( "d(u).a", t, "d(x+y^2)", "Dif(Add(Var(x),Pow(Var(y),Num(2))))" )    
     par( "dif.h", t, "z^4*y^3*2*x*dx+z^4*x^2*3*y^2*dy+x^2*y^3*4*z^3*dz" )    
     lam( "dif.i",   t, "y*dx+x*dy", "Add(Mul(Var(y),Dif(Var(x))),Mul(Var(x),Dif(Var(y))))" ) 
 
   }
 
  def testItg()
  {
    val t = new Text(200)
    
    itg( "itg.a", t, "dx",      "x"       ) 
    itg( "itg.b", t,  "x",      "x^2/2"   ) 
    itg( "itg.c", t,  "x^2",    "x^3/3"   )
    itg( "itg.d", t,  "sin(x)", "-cos(x)" )
    itg( "itg.e", t, "1/x",     "ln(x)"   ) 
  }
   
   def testFun()
   {
     val t = new Text(200)

     par(  "num.a", t, "(1+2)*3*(7-1)" ) 
     par(  "exp.a", t, "x^2+y^3*z^(a+4)" )  
     par(  "Ln.a",  t, "ln(x)" )   
     
     val sc = "ln(x)"
     Test.keep( "Lnn.b", "Lnn(Var(x))" )
       val ac:Exp = AsciiParse(sc)
           ac.lambda(t.x)
     Test.test( "Lnn.b", t )  
     
     lam(  "Lnn.c",   t, "ln(x)", "Lnn(Var(x))" )     
     par(  "Fun.a",  t, "sin(x)" )
     pars( "Fun.b",  t, "sin(x)", "cos(x)", "tan(x)", "cot(x)", "sec(x)", "csc(x)")          
     lam(  "Log.a",  t,  "log_10(x)", "Log(Var(x),Dbl(10.0))" )  // x*3-y^2
     lam(  "Root.a", t, "root_10(y)", "Roo(Var(y),Dbl(10.0))" )  // (y^5-z+1)       
     lam(  "Eee.a",  t, "e^x", "Eee(Var(x))" )      
     lam(  "Abs.a",  t, "|x-y|", "Abs(Sub(Var(x),Var(y)))" )      
     lam(  "Neg.a",  t, "-(x-y)", "Neg(Par(Sub(Var(x),Var(y))))" )       
     par(  "Big.a",  t, "[a^3,b*i]+sin(x^2)*sqrt(y)-20*ln(z)" )
   }
  
  def testSum()
  {
     val t = new Text(200)
     
     par(  "Int.a", t, "Int_0^x(x)" ) // "Int sub(0)sup(x)(x)" 
     par(  "sum.b", t, "sum_1^n(x^2)" )
     pars( "Int.c", t, "Int_1^n(x)", "Int_1^x(x)" )    
     pars( "sum.f", t, "sum_1^n(i)" )
     pars( "Int,g", t, "Int_1^x(x)" )     
     pars( "sum.h", t, "sum_{i=1}^n(x)", "sum_1^x(x)" )
  }
  
  def testEqu()
  {
    val t = new Text(200)
     
    lam( "Equ.a", t, "a+b=c+f", "Equ(Add(Var(a),Var(b)),Add(Var(c),Var(f)))" )
    par( "Equ.b", t, "(x^2)=3*x" )    
    lam( "Equ.c", t, "tan(x)=sin(x)/cos(x)", "Equ(Tan(Var(x)),Div(Sin(Var(x)),Cos(Var(x))))" )
    lam( "Equ.d", t, "cot(x)=cos(x)/sin(x)", "Equ(Cot(Var(x)),Div(Cos(Var(x)),Sin(Var(x))))" )       
    par( "Equ.e", t, "sin(x^2)=[a^3,b*i]" )
    par( "Equ.f", t, "sin(x^2)=[[a^3,b*i][x,y]]" )  
  }

  def testErr()
  {
     val t = new Text(200)
     val sa = "e&x$w~t@"
     Test.keep( "Err.a", "error", ' ', sa )
       val ea:Exp = AsciiParse(sa)
           ea.ascii(t.x)
     Test.test( "Err.a", t,      ' ', sa  ) 
   
     par( "Err.b", t, "sin(x" )     
     par( "Err.c", t, "haha(x)" )
  }
  
  def testSim()
  {
    val t = new Text(200)
    sim( "Sim.a", t, "(x+y)/(x+y)",          "1" )
    sim( "Sim.b", t, "(x+y)^3/(x+y)^3",      "1" )
    sim( "Sim.c", t,  "(x+y)*(a+b)/(x+y)",    "a+b" )
    sim( "Sim.d", t, "((x+y)*(a+b))/(x+y)",   "a+b" )
    sim( "Sim.e", t, "(x+y)/((x+y)*(a+b))",  "1/(a+b)" )   
    sim( "Sim.f", t, "(w*x*y*z)/(x*y*z)",    "w" )    
    sim( "Sim.g", t, "(x*y*z)/(x*y*z*w)",    "1/w" ) 
    sim( "Sim.h", t, "(w*x*y*z)/(z*x*w)",    "y" ) 
    sim( "Sim.i", t, "(w*x*y*x)/(z*x*w)",    "y*x/z" )
    sim( "Sim.j", t, "((w-q)*x*y*x)/(z*x*w*(w-q))",     "y*x/(z*w)" )    
    sim( "Sim.k", t, "((w-q)^3*x*y*x)/(z*x*w*(w-q)^2)", "(w-q)*y*x/(z*w)" )
  }
  
  
// Right now subscript and superscript are a problem with stack overflows  
   def testSus()
   {
     val t = new Text(200)
     lam(  "sus.a", t, "x_1", "Sus(Var(x),Num(1))" )
     lam(  "sus.b", t, "x_y", "Sus(Var(x),Var(y))" )
   }
     
 
}