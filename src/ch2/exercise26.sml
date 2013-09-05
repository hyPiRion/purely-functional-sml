(* Unbalanced Set *)

exception NotFound

signature FiniteMap =
sig
    type Key
    type 'a Map

    val empty  : 'a Map
    val bind   : Key * 'a * 'a Map -> 'a Map
    val lookup : Key * 'a Map -> 'a (* Raise NotFound if not found *)
end

signature Ordered =
sig
    type T

    val eq :  T * T -> bool
    val lt :  T * T -> bool
    val leq : T * T -> bool
end

functor UnbalancedMap (Key: Ordered, Value : 'a): FiniteMap =
struct
    type K = Element.T
    type V = 'a.T
    datatype Tree = E | T of Tree * K * V * Tree
    type FiniteMap = Tree

    val empty = E

    fun bind (k, v, E) = T (E, K, V, E)
      | bind (k, v, s) = raise NotFound (* TODO *)

    fun lookup (k, E) = raise NotFound
      | lookup (k, T (a, k, v, b)) = v
      | lookup (k, T (a, k2, v, b)) = raise NotFound

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
end
