type t

val init : float -> t

val get_val : t -> float

val get_grad : t -> float

val ( +! ) : t -> t -> t

val ( *! ) : t -> t -> t

val string_history : t -> string

