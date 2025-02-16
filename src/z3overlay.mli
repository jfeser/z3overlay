open! Base

module type Context = sig
  val ctx : Z3.context
end

module Make (C : Context) : sig
  module Type : sig
    type ('a, 'b) t =
      | Int : (Z.t, [ `Int ]) t
      | Real : (Q.t, [ `Real ]) t
      | Bool : (bool, [ `Bool ]) t
      | Pseudo_bool : (bool, [ `Pseudo_bool ]) t
      | Tuple_2 :
          ('a1, 'b1) t * ('a2, 'b2) t
          -> ('a1 * 'a2, [ `Tuple of 'b1 * 'b2 ]) t
      | Array :
          ('a1, 'b1) t * ('a2, 'b2) t
          -> ('a1 -> 'a2, [ `Array of 'b1 * 'b2 ]) t

    val sexp_of_t : (_, _) t -> Sexp.t
    val compare : ('a, 'b) t -> ('a, 'b) t -> int
    val to_sort : (_, _) t -> Z3.Sort.sort
  end

  module Status : sig
    type t = Unsat | Sat of Z3.Model.model option lazy_t | Unknown of string
    [@@deriving sexp_of]
  end

  module rec Term : sig
    type ('a, 'b) t

    val sexp_of_t : (_, _) t -> Sexp.t
    val compare : ('a, 'b) t -> ('a, 'b) t -> int
    val simplify : ?params:Z3.Params.params -> ('a, 'b) t -> ('a, 'b) t
    val const : ('a, 'b) Type.t -> string -> ('a, 'b) t
    val ( = ) : ('a, 'b) t -> ('a, 'b) t -> (bool, [ `Bool ]) Term.t
    val distinct : (_, _) t list -> (bool, [ `Bool ]) Term.t
    val to_string : (_, _) t -> string

    val forall :
      ('a, 'b) Type.t -> (('a, 'b) t -> (bool, [ `Bool ]) t) -> (bool, [ `Bool ]) t

    val exists :
      ('a, 'b) Type.t -> (('a, 'b) t -> (bool, [ `Bool ]) t) -> (bool, [ `Bool ]) t
  end

  module type Value = sig
    type value
    type kind
    type t = (value, kind) Term.t
  end

  module Bool : sig
    include Value with type value = bool and type kind = [ `Bool ]

    val const : string -> t
    val true_ : t
    val false_ : t
    val bool : bool -> t
    val not : t -> t
    val ite : t -> ('a, 'b) Term.t -> ('a, 'b) Term.t -> ('a, 'b) Term.t
    val ( <=> ) : t -> t -> t
    val ( ==> ) : t -> t -> t
    val xor : t -> t -> t
    val and_ : t list -> t
    val ( && ) : t -> t -> t
    val or_ : t list -> t
    val ( || ) : t -> t -> t
  end

  module Int : sig
    include Value with type value = Z.t and type kind = [ `Int ]
    include Num_intf.S with type t := t and type bool := Bool.t

    val const : string -> t
    val int : int -> t
    val bigint : Z.t -> t
    val ( mod ) : t -> t -> t
    val rem : t -> t -> t
  end

  module Real : sig
    include Value with type value = Q.t and type kind = [ `Real ]
    include Num_intf.S with type t := t and type bool := Bool.t

    val const : string -> t
    val of_int : Int.t -> t
    val to_int : t -> Int.t
  end

  module Array : sig
    type ('a1, 'a2, 'b1, 'b2) t = ('a1 -> 'a2, [ `Array of 'b1 * 'b2 ]) Term.t

    val const :
      ('a1, 'b1) Type.t -> ('a2, 'b2) Type.t -> string -> ('a1, 'a2, 'b1, 'b2) t

    val get : ('a1, 'a2, 'b1, 'b2) t -> ('a1, 'b1) Term.t -> ('a2, 'b2) Term.t

    val set :
      ('a1, 'a2, 'b1, 'b2) t ->
      ('a1, 'b1) Term.t ->
      ('a2, 'b2) Term.t ->
      ('a1, 'a2, 'b1, 'b2) t

    module M (A : Value) (B : Value) :
      Value
        with type value = A.value -> B.value
         and type kind = [ `Array of A.kind * B.kind ]
  end

  module T2 : sig
    type ('a1, 'a2, 'b1, 'b2) t = ('a1 * 'a2, [ `Tuple of 'b1 * 'b2 ]) Term.t

    val create : ('a1, 'b1) Term.t -> ('a2, 'b2) Term.t -> ('a1, 'a2, 'b1, 'b2) t
    val get1 : ('a, _, 'b, _) t -> ('a, 'b) Term.t
    val get2 : (_, 'a, _, 'b) t -> ('a, 'b) Term.t
    val of_tuple : ('a1, 'b1) Term.t * ('a2, 'b2) Term.t -> ('a1, 'a2, 'b1, 'b2) t
    val to_tuple : ('a1, 'a2, 'b1, 'b2) t -> ('a1, 'b1) Term.t * ('a2, 'b2) Term.t

    module M (A : Value) (B : Value) :
      Value
        with type value = A.value * B.value
         and type kind = [ `Tuple of A.kind * B.kind ]
  end

  module Model : sig
    type t = Z3.Model.model [@@deriving sexp_of]

    val eval : t -> ('a, _) Term.t -> 'a option
  end

  module Solver : sig
    module Params : sig
      type t [@@deriving sexp]

      val create : ?stats:bool -> ?timeout:int -> ?unsat_core:bool -> unit -> t
    end

    type t = Z3.Solver.solver [@@deriving sexp_of]

    val to_string : t -> string
    val create : ?params:Params.t -> unit -> t
    val push : t -> unit
    val pop : ?n:int -> t -> unit
    val reset : t -> unit
    val add : t -> Bool.t list -> unit
    val check : t -> Bool.t list -> Status.t
  end

  module Pseudo_bool : sig
    type t = (bool, [ `Pseudo_bool ]) Term.t

    val const : Solver.t -> string -> t
    val to_int : t -> Int.t
    val to_bool : t -> Bool.t
    val of_bool : Bool.t -> t
    val true_ : t
    val false_ : t
    val and_ : t list -> Bool.t
    val or_ : t list -> Bool.t
    val ( && ) : t -> t -> Bool.t
    val ( || ) : t -> t -> Bool.t
    val not : t -> t
    val ( ==> ) : t -> t -> Bool.t
  end

  module Optimizer : sig
    type t = Z3.Optimize.optimize

    val create : unit -> t
    val add : t -> Bool.t list -> unit
    val check : t -> Bool.t list -> Status.t
  end

  val bool : bool -> Bool.t
  val int : int -> Int.t
  val ( = ) : ('a, 'b) Term.t -> ('a, 'b) Term.t -> Bool.t
  val not : Bool.t -> Bool.t
  val ( <=> ) : Bool.t -> Bool.t -> Bool.t
  val ( ==> ) : Bool.t -> Bool.t -> Bool.t
  val ( && ) : Bool.t -> Bool.t -> Bool.t
  val ( || ) : Bool.t -> Bool.t -> Bool.t
  val ( < ) : Int.t -> Int.t -> Bool.t
  val ( <= ) : Int.t -> Int.t -> Bool.t
  val ( > ) : Int.t -> Int.t -> Bool.t
  val ( >= ) : Int.t -> Int.t -> Bool.t
  val ( ** ) : Int.t -> Int.t -> Int.t
  val ( + ) : Int.t -> Int.t -> Int.t
  val ( - ) : Int.t -> Int.t -> Int.t
  val ( * ) : Int.t -> Int.t -> Int.t
  val ( / ) : Int.t -> Int.t -> Int.t
  val ( ~- ) : Int.t -> Int.t
end
