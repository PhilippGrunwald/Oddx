type t

val init : float -> t

val get_val : t -> float

val get_grad : t -> float

val ( +! ) : t -> t -> t

val ( -! ) : t -> t -> t

val ( *! ) : t -> t -> t

val ( /! ) : t -> t -> t

val ( =! ) : t -> t -> bool

val oddx_exp : t -> t

val oddx_sin : t -> t

val oddx_cos : t -> t

val backward : t -> unit

val flush : t -> unit

val string_history : t -> string

