(* Unbalanced Set *)

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

    fun insert (x, E) = T (E, x, E)
      | insert (x, s as T (a, y, b)) =
          if Element.lt (x, y) then T (insert (x, a), y, b)
          else if Element.lt (y, x) then T (a, y, insert (x, b))
          else s

    (* 25 a) *)
    fun completeTree (x, 0) = E
      | completeTree (x, d : int) =
        let val child : Tree = completeTree (x, d - 1)
        in T (child, x, child)
        end

    (* 25 b) *)
    fun complete (x, 0) = E
      | complete (x, d : int) =
        let
            val leftChildSize = d div 2
            val leftChild = complete (x, leftChildSize)
        in
            if d - 2 * leftChildSize - 1 = 0 then
                T (leftChild, x, leftChild)
            else let val rightChild = complete (x, d - 1 - leftChildSize)
                 in T (leftChild, x, rightChild)
                 end
        end
end
