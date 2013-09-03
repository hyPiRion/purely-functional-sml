(* Excercise 2.1 *)
fun suffixes ([]) = [[]]
  | suffixes (xs) = xs :: suffixes (tl xs)
