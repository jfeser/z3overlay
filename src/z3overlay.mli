(** An overlay for the OCaml Z3 bindings. *)

open! Base

module type Context = sig
  val ctx : Z3.context
end

module Make (C : Context) : sig
  val ctx : Z3.context

  type zint = [`Int]

  type zbool = [`Bool]

  type zreal = [`Real]

  type znum = [zint | zreal]

  type zuntyped

  type ('domain, 'range) zarray = [`Zarray of 'domain * 'range]

  module Type : sig
    type (_, _) t =
      | Int : (Z.t, [> zint]) t
      | Bool : (bool, [> zbool]) t
      | Real : (Q.t, [> zreal]) t
      | Num : (Q.t, [> znum]) t
      | Array : ('a, 'x) t * ('b, 'y) t -> ('a -> 'b, ('x, 'y) zarray) t

    module Wrapped : sig
      type t

      type ('a, 'b) typ

      type 'c cont = {run: 'a 'b. ('a, 'b) typ -> 'c}

      val wrap : ('a, 'b) typ -> t

      val unwrap : 'c cont -> t -> 'c
    end
    with type ('a, 'b) typ := ('a, 'b) t
  end

  module rec Symbol : sig
    type ('a, 'b) t

    val get_typ : ('a, 'b) t -> ('a, 'b) Type.t

    val declare : ('a, 'b) Type.t -> string -> ('a, 'b) t

    val term : ('a, 'b) Type.t -> 'b Term.t -> ('a, 'b) t

    val trustme : ('a, 'b) Type.t -> Z3.Expr.expr -> ('a, 'b) t
    (** Unsafe cast. Use at your own risks. *)
  end

  (** Term constructors. Direct calls to the Z3 api. *)
  and Term : sig
    type +'a t

    type comparator_witness

    val comparator : ('a t, comparator_witness) Comparator.comparator

    module O : sig
      val ( >= ) : 'a t -> 'a t -> bool

      val ( <= ) : 'a t -> 'a t -> bool

      val ( = ) : 'a t -> 'a t -> bool

      val ( > ) : 'a t -> 'a t -> bool

      val ( < ) : 'a t -> 'a t -> bool

      val ( <> ) : 'a t -> 'a t -> bool
    end

    val sexp_of_t : 'a t -> Sexp.t

    val magic : 'a t -> 'b t

    val symbol : (_, 'a) Symbol.t -> 'a t

    val simplify : ?params:Z3.Params.params -> 'a t -> 'a t

    val eq : 'a t -> 'a t -> [> zbool] t

    val distinct : 'a t list -> [> zbool] t

    val ite : [< zbool] t -> 'a t -> 'a t -> 'a t

    val int : int -> [> zint] t

    val bigint : Z.t -> [> zint] t

    val rat : Q.t -> [> zreal] t

    val i2q : [< zint] t -> [> zreal] t

    val q2i : [< zreal] t -> [> zint] t

    val true_ : [> zbool] t

    val false_ : [> zbool] t

    val bool : bool -> [> zbool] t

    val and_ : [< zbool] t list -> [> zbool] t

    val or_ : [< zbool] t list -> [> zbool] t

    val not : [< zbool] t -> [> zbool] t

    val imply : [< zbool] t -> [< zbool] t -> [> zbool] t

    val iff : [< zbool] t -> [< zbool] t -> [> zbool] t

    val xor : [< zbool] t -> [< zbool] t -> [> zbool] t

    val ge : [< znum] t -> [< znum] t -> [> zbool] t

    val le : [< znum] t -> [< znum] t -> [> zbool] t

    val gt : [< znum] t -> [< znum] t -> [> zbool] t

    val lt : [< znum] t -> [< znum] t -> [> zbool] t

    val neg : ([< znum] as 'a) t -> 'a t

    val add : ([< znum] as 'a) t list -> 'a t

    val sub : ([< znum] as 'a) t list -> 'a t

    val mul : ([< znum] as 'a) t list -> 'a t

    val ixor : [< zint] t -> [< zint] t -> [> zint] t

    val div : ([< znum] as 'a) t -> 'a t -> 'a t

    val mod_ : [< zint] t -> [< zint] t -> [> zint] t

    val rem : [< znum] t -> [< znum] t -> [> znum] t

    val ( ! ) : (_, 'a) Symbol.t -> 'a t

    val ( = ) : 'a t -> 'a t -> [> zbool] t

    val ( <> ) : 'a t -> 'a t -> [> zbool] t

    val ( && ) : [< zbool] t -> [< zbool] t -> [> zbool] t

    val ( || ) : [< zbool] t -> [< zbool] t -> [> zbool] t

    val ( <=> ) : [< zbool] t -> [< zbool] t -> [> zbool] t

    val ( ==> ) : [< zbool] t -> [< zbool] t -> [> zbool] t

    val ( lxor ) : [< zbool] t -> [< zbool] t -> [> zbool] t

    val ( < ) : [< znum] t -> [< znum] t -> [> zbool] t

    val ( <= ) : [< znum] t -> [< znum] t -> [> zbool] t

    val ( > ) : [< znum] t -> [< znum] t -> [> zbool] t

    val ( >= ) : [< znum] t -> [< znum] t -> [> zbool] t

    val ( + ) : ([< znum] as 'a) t -> 'a t -> 'a t

    val ( - ) : ([< znum] as 'a) t -> 'a t -> 'a t

    val ( * ) : ([< znum] as 'a) t -> 'a t -> 'a t

    val ( / ) : ([< znum] as 'a) t -> 'a t -> 'a t

    val ( mod ) : [< zint] t -> [< zint] t -> [> zint] t

    val with_typ : ('a, 'b) Type.t -> 'a -> 'b t

    val subst : _ t -> (_ t * _ t) list -> zuntyped t

    val to_string : 'a t -> string

    val raw : 'a t -> Z3.Expr.expr
  end

  module Z3Array : sig
    val get : [< ('d, 'r) zarray] Term.t -> 'd Term.t -> 'r Term.t

    val set :
         [< ('d, 'r) zarray] Term.t
      -> 'd Term.t
      -> 'r Term.t
      -> [> ('d, 'r) zarray] Term.t

    val make :
      ('a -> 'b, ('d, 'r) zarray) Type.t -> 'r Term.t -> [> ('d, 'r) zarray] Term.t

    val default : [< ('d, 'r) zarray] Term.t -> 'r Term.t

    val of_indexed :
         typ:('a, 'r) Type.t
      -> default:'r Term.t
      -> 'r Term.t array
      -> ([> zint], 'r) zarray Term.t

    val of_array :
         typ:('a -> 'b, ('d, 'r) zarray) Type.t
      -> default:'r Term.t
      -> ('d Term.t * 'r Term.t) array
      -> ('d, 'r) zarray Term.t

    val of_list :
         typ:('a -> 'b, ('d, 'r) zarray) Type.t
      -> default:'r Term.t
      -> ('d Term.t * 'r Term.t) list
      -> ('d, 'r) zarray Term.t
  end

  type sat =
    | Unsat of zuntyped Term.t Lazy.t  (** Proof *)
    | Sat of Z3.Model.model Lazy.t  (** Model *)
    | Unknown of string  (** Reason *)

  module type SOLVER = sig
    type t

    val make : unit -> t

    val push : t -> unit

    val pop : t -> unit

    val add : solver:t -> [> zbool] Term.t -> unit

    val check : solver:t -> [> zbool] Term.t list -> sat

    val to_string : t -> string
  end

  module Solver : SOLVER with type t = Z3.Solver.solver

  module Optimize : sig
    include SOLVER with type t = Z3.Optimize.optimize

    type handle

    val add_soft :
      id:Z3.Symbol.symbol -> solver:t -> weight:string -> zbool Term.t -> handle

    val maximize : solver:t -> [< znum] Term.t -> handle

    val minimize : solver:t -> [< znum] Term.t -> handle

    val get_upper : handle -> (Q.t, [> znum]) Symbol.t

    val get_lower : handle -> (Q.t, [> znum]) Symbol.t
  end

  module Model : sig
    val get_value : model:Z3.Model.model -> ('a, 'b) Symbol.t -> 'a

    val eval : model:Z3.Model.model -> 'a Term.t -> 'a Term.t

    val to_string : Z3.Model.model -> string
  end
end
