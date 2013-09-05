(* Exercise 2.4 - Merge 2.2 and 2.3. *)

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

    local
        fun fastMember (x, E, acc) = Element.eq (x, acc)
          | fastMember (x, T (a, y, b), acc) =
            if Element.lt (y, x) then fastMember (x, b, acc)
            else fastMember(x, a, y)
    in
    fun member (x, E) = false
      | member (x, s as T (_, y, _)) =
        fastMember(x, s, y)
    end
                   
    local
        exception IsMember
        fun fastInsert (x, E, acc) =
            if Element.eq(x, acc) then raise IsMember 
            else T (E, x, E)
          | fastInsert (x, s as T (a, y, b), acc) =
            if Element.lt (y, x) then
                let val b' = fastInsert (x, b, acc) in
                    T (a, y, b')
                end
            else
                let val a' = fastInsert (x, a, y) in
                    T (a', y, b)
                end
    in
    fun insert (x, E) = T (E, x, E)
      | insert (x, s as T (_, y, _)) =
        fastInsert (x, s, y)
        handle IsMember => s
    end
end
