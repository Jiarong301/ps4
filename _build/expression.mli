(*
                         CS 51 Problem Set 2
                 A Language for Symbolic Mathematics
                             Spring 2018
 *)

open ExpressionLibrary ;;

val contains_var : expression -> bool 
val evaluate : expression -> float -> float 
val derivative : expression -> expression

val find_zero : expression -> float -> float -> int -> float option

val find_zero_exact : expression -> expression option

val minutes_spent_on_part2 : unit -> int