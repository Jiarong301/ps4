(* 
                         CS 51 Problem Set 4
                 A Language for Symbolic Mathematics
                               Testing
 *)

open Expression ;;
open ExpressionLibrary ;;

open Test_simple ;;      (* a really simple unit testing framework *)
  
let test () =
  unit_test (contains_var (parse "5-1+(~x)")) "contains_var sub add neg";
  unit_test (contains_var (parse "(3+2^x)/5")) "contains_var add power";
  unit_test (not (contains_var (parse "cos(5+5/2)"))) "num cos";
  unit_test (not (contains_var (parse "ln(5^2)"))) "num ln";

  unit_test ((evaluate (parse "x^4 + 3") 2.0) = 19.0) "evaluate sum pow";
  unit_test ((evaluate (parse "~x*5 - 2") 2.0) = ~-. 12.0) "evaluate mul sub neg";
  unit_test ((evaluate (parse "ln(cos(x))") 0.) = 0.) "evaluate ln cos";

  
  unit_test ((derivative (parse "4*x^3")) = (parse "((3.*((2.*(x^(2.-1.)))*1.))+(0.*(x^2.)))")) "derivative pow mul";

  unit_test ((evaluate (derivative (parse "4*x^2+2")) 2.) = (evaluate (parse "8*x") 2.)) "derivative pow(no var) mul sum";
  unit_test ((evaluate (derivative (parse "~4/x")) 4.) = (evaluate (parse "4*x^(~2)") 4.)) "derivative neg div";
  unit_test ((evaluate (derivative (parse "sin(cos(x))")) 6.) = (evaluate (parse "~cos(cos(x))*sin(x)") 6.)) "derivative cos sin";
  unit_test ((evaluate (derivative (parse "(x+5)^(x+6)")) 3.) = (evaluate (parse "(x+5)^(x+6)*(ln(x+5)+1)") 3.)) "derivative pow(var)";;

  () ;;

test ();;
