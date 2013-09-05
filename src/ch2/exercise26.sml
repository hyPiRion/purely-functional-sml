(* Unbalanced Set *)

signature Ordered =
sig
    type T

    val eq :  T * T -> bool
    val lt :  T * T -> bool
    val leq : T * T -> bool
end

exception NotFound

signature FiniteMap =
sig
    type Key
    type 'a Map

    val empty  : 'a Map
    val bind   : Key * 'a * 'a Map -> 'a Map
    val lookup : Key * 'a Map -> 'a (* Raise NotFound if not found *)
end

functor UnbalancedMap (K : Ordered) : FiniteMap =
struct
    type Key = K.T
    datatype 'a Map = E | T of 'a Map * Key * 'a * 'a Map
    type FiniteMap = 'a Map

    val empty = E

    fun bind (k, v, E) = T (E, k, v, E)
      | bind (k, v, s : FiniteMap) = raise NotFound (* TODO *)

    fun lookup (k, E) = raise NotFound
      | lookup (k, T (a, k2, v, b)) = v
end
