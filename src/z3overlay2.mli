(* open! Base
 * 
 * module type Context = sig
 *   val ctx : Z3.context
 * end
 * 
 * module Make (C : Context) : sig
 *   module Type : sig
 *     type pint = PInt
 * 
 *     type preal = PReal
 * 
 *     type pbool = PBool
 * 
 *     type pstring = PString
 * 
 *     type pfloat = PFloat
 * 
 *     type ('a, 'b) parray = PArray of 'a * 'b
 * 
 *     type pbv = PBV of int
 * 
 *     type 'a pseq = PSeq of 'a
 * 
 *     type 'a pset = PSet of 'a
 * 
 *     class type ['a] penum =
 *       object
 *         method n_values : int
 * 
 *         method value : int -> 'a sexp_option
 *       end
 * 
 *     type ('a, 'b) t =
 *       | Int : (Z.t, pint) t
 *       | Real : (Q.t, preal) t
 *       | Bool : (bool, pbool) t
 *       | String : (string, pstring) t
 *       | Float : {exp_bits: int; sig_bits: int} -> (Q.t, pfloat) t
 *       | Array : ('a, 'x) t * ('b, 'y) t -> ('a -> 'b, ('x, 'y) parray) t
 *       | Bitvector : int -> (int, pbv) t
 *       | Set : ('a, 'b) t -> ('a -> bool, 'b pset) t
 * 
 *     val to_sort : ('a, 'b) t -> Z3.Sort.sort
 * 
 *     val unify : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t Or_error.t
 *   end
 * 
 *   module rec Term : sig
 *     type ('a, 'b) t
 * 
 *     val simplify : ?params:Z3.Params.params -> ('a, 'b) t -> ('a, 'b) t
 * 
 *     val const : ('a, 'b) Type.t -> string -> ('a, 'b) t
 * 
 *     val ( = ) : ('a, 'b) t -> ('a, 'b) t -> Bool.t
 * 
 *     val distinct : ('a, 'b) t list -> Bool.t
 * 
 *     val to_string : ('a, 'b) t -> string
 *   end
 * 
 *   and Bool : sig
 *     type t = (bool, Type.pbool) Term.t
 * 
 *     val const : string -> t
 * 
 *     val true_ : t
 * 
 *     val false_ : t
 * 
 *     val bool : bool -> t
 * 
 *     val not : t -> t
 * 
 *     val ite : t -> ('a, 'b) Term.t -> ('a, 'b) Term.t -> ('a, 'b) Term.t
 * 
 *     val ( <=> ) : t -> t -> t
 * 
 *     val ( ==> ) : t -> t -> t
 * 
 *     val xor : t -> t -> t
 * 
 *     val and_ : t list -> t
 * 
 *     val ( && ) : t -> t -> t
 * 
 *     val or_ : t list -> t
 * 
 *     val ( || ) : t -> t -> t
 *   end
 * 
 *   and Int : sig
 *     type t = (Z.t, Type.pint) Term.t
 * 
 *     include Num_intf.S with type t := t and type bool := Bool.t
 * 
 *     val const : string -> t
 * 
 *     val int : int -> t
 * 
 *     val bigint : Z.t -> t
 * 
 *     val ( mod ) : t -> t -> t
 * 
 *     val rem : t -> t -> t
 * 
 *     val to_real : t -> Real.t
 *   end
 * 
 *   and Real : sig
 *     type t = (Q.t, Type.preal) Term.t
 * 
 *     include Num_intf.S with type t := t and type bool := Bool.t
 * 
 *     val const : string -> t
 * 
 *     val to_int : t -> Int.t
 *   end
 * end *)
