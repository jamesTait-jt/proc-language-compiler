import CW2

main = do
  print (s_scopetest      == 5)
  print (s_factest        == 6)
  print (s_whiletest      == 6)
  print (s_mutualtestOdd  == 0)
  print (s_mutualtestEven == 1)
  print (d_scopetest      == 6)
  print (d_factest        == 6)
  print (d_whiletest      == 6)
  print (d_mutualtestOdd  == 0)
  print (d_mutualtestEven == 1)
  print (m_scopetest      == 10)
  print (m_factest        == 6)
  print (m_whiletest      == 6)
  print (m_mutualtestOdd  == 0)
  print (m_mutualtestEven == 1)


s_scopetest :: Z
s_scopetest = (s_static (parse "begin var x:=0; proc p is x:=x*2; proc q is call p; begin var x:=5; proc p is x:=x+1; call q; y:=x end end") s_initialStateOdd) "y"

s_factest :: Z
s_factest = (s_static (parse "begin proc fac is begin var z:=x; if x=1 then skip else x:=x-1; call fac; y:=z*y end; y:=1; call fac end") s_initialStateOdd) "y"

s_whiletest :: Z
s_whiletest = (s_static (parse " y:=1; while !(x=1) do ( y:=y*x; x:=x-1 )") s_initialStateOdd) "y"

s_mutualtestOdd :: Z
s_mutualtestOdd = (s_static (parse "/* blam */ begin proc even is (begin (if x = 0) then (y:= 1) else x := x -1; call odd end); proc odd is (begin (if (x = 0) then (y:= 0) else (x := x-1); call even end); call even end") s_initialStateOdd) "y"

s_mutualtestEven :: Z
s_mutualtestEven = (s_static (parse "/* blam */ begin proc even is begin if x = 0 then y:= 1 else x := x -1; call odd end; proc odd is begin if x = 0 then y:= 0 else x := x-1; call even end; call even end") s_initialStateEven) "y"


s_initialStateOdd :: State
s_initialStateOdd "x" = 3
s_initialStateOdd  _  = 0

s_initialStateEven :: State
s_initialStateEven "x" = 4
s_initialStateEven  _  = 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

m_scopetest :: Z
m_scopetest = (s_mixed (parse "begin var x:=0; proc p is x:=x*2; proc q is call p; begin var x:=5; proc p is x:=x+1; call q; y:=x end end") m_initialStateOdd) "y"

m_factest :: Z
m_factest = (s_mixed (parse "begin proc fac is begin var z:=x; if x=1 then skip else x:=x-1; call fac; y:=z*y end; y:=1; call fac end") m_initialStateOdd) "y"

m_whiletest :: Z
m_whiletest = (s_mixed (parse " y:=1; while !(x=1) do ( y:=y*x; x:=x-1 )") m_initialStateOdd) "y"

m_mutualtestOdd :: Z
m_mutualtestOdd = (s_mixed (parse "/* blam */ begin proc even is begin if x = 0 then y:= 1 else x := x -1; call odd end; proc odd is begin if x = 0 then y:= 0 else x := x-1; call even end; call even end") m_initialStateOdd) "y"

m_mutualtestEven :: Z
m_mutualtestEven = (s_mixed (parse "/* blam */ begin proc even is begin if x = 0 then y:= 1 else x := x -1; call odd end; proc odd is begin if x = 0 then y:= 0 else x := x-1; call even end; call even end") m_initialStateEven) "y"

m_initialStateOdd :: State
m_initialStateOdd "x" = 3
m_initialStateOdd  _  = 0

m_initialStateEven :: State
m_initialStateEven "x" = 4
m_initialStateEven  _  = 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

d_scopetest :: Z
d_scopetest = (s_dynamic (parse "begin var x:=0; proc p is x:=x*2; proc q is call p; begin var x:=5; proc p is x:=x+1; call q; y:=x end end") d_initialStateOdd) "y"

d_factest :: Z
d_factest = (s_dynamic (parse "begin proc fac is begin var z:=x; if x=1 then skip else x:=x-1; call fac; y:=z*y end; y:=1; call fac end") d_initialStateOdd) "y"

d_whiletest :: Z
d_whiletest = (s_dynamic (parse " y:=1; while !(x=1) do ( y:=y*x; x:=x-1 )") d_initialStateOdd) "y"

d_mutualtestOdd :: Z
d_mutualtestOdd = (s_dynamic (parse "/* blam */ begin proc even is begin if x = 0 then y:= 1 else x := x -1; call odd end; proc odd is begin if x = 0 then y:= 0 else x := x-1; call even end; call even end") d_initialStateOdd) "y"

d_mutualtestEven :: Z
d_mutualtestEven = (s_dynamic (parse "/* blam */ begin proc even is begin if x = 0 then y:= 1 else x := x -1; call odd end; proc odd is begin if x = 0 then y:= 0 else x := x-1; call even end; call even end") d_initialStateEven) "y"


d_initialStateOdd :: State
d_initialStateOdd "x" = 3
d_initialStateOdd  _  = 0

d_initialStateEven :: State
d_initialStateEven "x" = 4
d_initialStateEven  _  = 0
