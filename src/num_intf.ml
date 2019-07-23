module type S = sig
  type t

  type bool

  val add : t list -> t

  val sub : t list -> t

  val mul : t list -> t

  val ( < ) : t -> t -> bool

  val ( <= ) : t -> t -> bool

  val ( > ) : t -> t -> bool

  val ( >= ) : t -> t -> bool

  val ( ** ) : t -> t -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : t -> t -> t

  val ( / ) : t -> t -> t

  val ( ~- ) : t -> t
end
