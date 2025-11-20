

(* type point = { x : float; y : float; z : float }

type atom   (* abstract type *)

(* public constructor *)
val create : point -> string -> atom

(* accessors *)
val coords : atom -> point
val name : atom -> string
val form_fact : atom -> float -> Complex.t *)