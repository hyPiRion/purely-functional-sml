(* Exercise 2.2 - Rewrite `member` to take no more than d + 1 comparisons. *)

signature Set =
sig
    type Elem
    type Set

    val empty  : Set
    val insert : Elem * Set -> Set
    val member : Elem * Set -> bool
end

signature Ordered =
sig
    type T

    val eq :  T * T -> bool
    val lt :  T * T -> bool
    val leq : T * T -> bool
end

functor UnbalancedSet (Element: Ordered): Set =
struct
    type Elem = Element.T
    datatype Tree = E | T of Tree * Elem * Tree
    type Set = Tree
    
    val empty = E
    
    fun member (x, E) = false
      | member (x, T (a, y, b)) =
        let (* Max d comparisons in here. *)
            fun accmember (acc, E) = Element.eq (acc, x)
              | accmember (acc, T (a, y, b)) =
                if Element.lt (y, x) then accmember (acc, b)
                else accmember (y, a)
        in  (* 1 comparison here *)
            if Element.lt (y, x) then accmember (y, b)
                                     (* y != x, but need elt *)
            else accmember (y, a)
        end
    
    fun insert (x, E) = T (E, x, E)
      | insert (x, s as T (a, y, b)) =
          if Element.lt (x, y) then T (insert (x, a), y, b)
          else if Element.lt (y, x) then T (a, y, insert (x, b))
          else s
end
