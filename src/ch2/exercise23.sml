(* Exercise 2.3 - Rewrite `insert` to avoid path copying for existing elts.  *)

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
          if Element.lt (x, y) then member (x, a)
          else if Element.lt (y, x) then member (x, b)
          else true
                   
    local
        exception IsMember
        fun exceptionInsert (x, E) = T (E, x, E)
          | exceptionInsert (x, s as T (a, y, b)) =
            if Element.lt (x, y) then
                let val a' = exceptionInsert (x, a) in
                    T (a', y, b)
                end
            else if Element.lt (y, x) then
                let val b' = exceptionInsert (x, b) in
                    T (a, y, b')
                end
            else raise IsMember
    in
    fun insert (x, E) = T (E, x, E)
      | insert (x, s) =
        exceptionInsert (x, s)
        handle IsMember => s
    end
end
