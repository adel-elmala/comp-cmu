datatype exp = 
Constant of int 
| Negate of exp
| Add of exp * exp
| Multiply of exp * exp



val e1 = Add ( Constant 5 , Negate (Constant 6) )

val e2 = Multiply ( Negate(Constant 63) , Negate (Constant 63) )

val e3 = Multiply ( Constant 5 , Negate (Constant 5) )


fun eval (e:exp) : int = 
    case e of 
          Constant i => i 
        | Negate e1 => ~ (eval e1) 
        | Add (e1,e2) => (eval e1) + (eval e2)
        | Multiply (e1,e2) => (eval e1)* (eval e2)