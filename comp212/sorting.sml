(* insert an element [elt] to the right position in [lst]
Example : insert(3,[1,2,4]) -> [1,2,3,4]
 *)


 fun insert (elt:int, lst:int list):int list =
    case lst of
        [] => elt::[]
        | h::t => if elt < h then (elt::lst) else h::insert(elt, t)  



fun isort (lst : int list ) : int list =
    case lst of 
    [] => []
    | h::t => insert(h,isort(t))       

fun split (l:int list) : int list * int list = 
    case l of  
         [] => ([],[])
        | [x] => ([x],[])
        | h::h'::t' => let val (left,right) = split t'
                        in (h::left , h'::right)
                        end


 fun merge (pile1 ,pile2) : int list = 
    case (pile1,pile2) of 
     ([],pile2) => pile2 
    | (pile1,[]) => pile1
    | (h1::t1 , h2::t2) => if h1 < h2 then h1::merge (t1, pile2) else h2:: merge (pile1, t2)  

fun mergesort (l :int list) : int list = 
    case l of 
    [] => []
    | [x] => [x] 
    | _ =>
        let val (p1 , p2) = split l 
        in merge(mergesort p1 ,mergesort p2)
        end

val sp = split([9,23,6,7,3,1,0,5])    
val mrg = merge ([1,6,7,9],[2,3,4,6,7,9])




