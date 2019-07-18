open! Base
open Z3

let opt_get = function None -> raise @@ Z3.Error "opt_get" | Some x -> x

module type Context = sig
  val ctx : Z3.context
end

module Make (C : Context) = struct
  let ctx = C.ctx

  type zint = [`Int]

  type zbool = [`Bool]

  type zreal = [`Real]

  type znum = [zint | zreal]

  type zuntyped

  type ('domain, 'range) zarray = [`Zarray of 'domain * 'range]

  module Type = struct
    type (_, _) t =
      | Int : (Z.t, [> zint]) t
      | Bool : (bool, [> zbool]) t
      | Real : (Q.t, [> zreal]) t
      | Num : (Q.t, [> znum]) t
      | Array : ('a, 'x) t * ('b, 'y) t -> ('a -> 'b, ('x, 'y) zarray) t

    module Wrapped = struct
      type ('a, 'b) typ = ('a, 'b) t

      type t = Wrap : ('a, 'b) typ -> t

      type 'c cont = {run: 'a 'b. ('a, 'b) typ -> 'c}

      let wrap x = Wrap x

      let unwrap k (Wrap x) = k.run x
    end
  end

  (* module Term2 = struct
   *   module Type = struct
   *     type pint = PInt
   * 
   *     type preal = PReal
   * 
   *     type pbool = PBool
   * 
   *     type pstring = PString
   * 
   *     type pfloat = PFloat of {exp_bits: int; sig_bits: int}
   * 
   *     type ('a, 'b) parray = PArray of 'a * 'b
   * 
   *     type pbv = PBV of int
   * 
   *     type 'a pseq = PSeq of 'a
   * 
   *     type nothing
   * 
   *     type 'a pset = PSet of 'a
   * 
   *     class type ['a] penum =
   *       object
   *         method n_values : int
   * 
   *         method value : int -> 'a option
   *       end
   * 
   *     type ('a, 'b) t =
   *       | Int : (Z.t, pint) t
   *       | Real : (Q.t, preal) t
   *       | Bool : (bool, pbool) t
   *       | String : (string, pstring) t
   *       | Float : (Q.t, pfloat) t
   *       | Array : ('a, 'x) t * ('b, 'y) t -> ('a -> 'b, ('x, 'y) parray) t
   *       | Enum : ('a #penum as 'b) -> ('a, 'b) t
   *       | Bitvector : int -> (int, pbv) t
   *       | Set : ('a, 'b) t -> (nothing, 'b pset) t
   * 
   *     type zint = (Z.t, pint) t
   * 
   *     type zreal = (Q.t, preal) t
   * 
   *     type zbool = (bool, pbool) t
   * 
   *     type zstring = (string, pstring) t
   * 
   *     type zfloat = (Q.t, pfloat) t
   * 
   *     type ('a, 'b, 'x, 'y) zarray = ('a -> 'b, ('x, 'y) parray) t
   * 
   *     type ('a, 'b) zenum = ('a, 'b) t constraint 'b = 'a #penum
   * 
   *     type zbv = (int, pbv) t
   * 
   *     type 'b zset = (nothing, 'b pset) t
   * 
   *     let rec to_sort : type a b. (a, b) t -> Sort.sort = function
   *       | Int -> Arithmetic.Integer.mk_sort ctx
   *       | Bool -> Boolean.mk_sort ctx
   *       | Real -> Arithmetic.Real.mk_sort ctx
   *       | Num -> Arithmetic.Real.mk_sort ctx
   *       | Array (src, dst) -> Z3Array.mk_sort ctx (sort src) (sort dst)
   *   end *)

  (*   module Term : sig
   *     type 'a t
   * 
   *     val true_ : Type.zbool t
   * 
   *     val false_ : Type.zbool t
   * 
   *     val not : Type.zbool t -> Type.zbool t
   *   end = struct
   *     type 'a t = 'a * Z3.Expr.expr
   * 
   *     let true_ = (Type.Bool, Boolean.mk_true ctx)
   * 
   *     let false_ = (Type.Bool, Boolean.mk_false ctx)
   * 
   *     let not (_, x) = (Type.Bool, Boolean.mk_not ctx x)
   * 
   *     let empty t = (Type.Set t, Set.mk_empty ctx)
   *   end
   * end *)

  module Term = struct
    module T = struct
      type +'a t = Z3.Expr.expr

      let compare = Z3.Expr.compare

      let sexp_of_t t = Sexp.Atom Z3.(Expr.ast_of_expr t |> AST.to_sexpr)
    end

    include T
    module C = Comparator.Make1 (T)
    include C

    module Cmp = Comparable.Make_using_comparator (struct
      include C

      type t = Expr.expr

      let sexp_of_t = sexp_of_t
    end)

    module O = struct
      include (Cmp : Comparable.Infix with type t := Expr.expr)
    end

    let magic t = t

    let symbol s = snd s

    let eq x y = Boolean.mk_eq ctx x y

    let distinct x = Boolean.mk_distinct ctx x

    let ite b then_ else_ = Boolean.mk_ite ctx b then_ else_

    let true_ = Boolean.mk_true ctx

    let false_ = Boolean.mk_false ctx

    let bool b = if b then true_ else false_

    let and_ t = Boolean.mk_and ctx t

    let or_ t = Boolean.mk_or ctx t

    let not t = Boolean.mk_not ctx t

    let imply t1 t2 = Boolean.mk_implies ctx t1 t2

    let iff t1 t2 = Boolean.mk_iff ctx t1 t2

    let xor t1 t2 = Boolean.mk_xor ctx t1 t2

    let ge t1 t2 = Arithmetic.mk_ge ctx t1 t2

    let le t1 t2 = Arithmetic.mk_le ctx t1 t2

    let gt t1 t2 = Arithmetic.mk_gt ctx t1 t2

    let lt t1 t2 = Arithmetic.mk_lt ctx t1 t2

    (* We go through strings since we would loss precision otherwise. *)
    let int i = Arithmetic.Integer.mk_numeral_i ctx i

    let bigint i = Arithmetic.Integer.mk_numeral_s ctx @@ Z.to_string i

    let rat x =
      let s = Q.to_string x in
      try Arithmetic.Real.mk_numeral_s ctx s
      with _ -> failwith (Printf.sprintf "Real.mk_numeral_s parse error on %s." s)

    let i2q t = Arithmetic.Integer.mk_int2real ctx t

    let q2i t = Arithmetic.Real.mk_real2int ctx t

    let neg t = Arithmetic.mk_unary_minus ctx t

    let add t = Arithmetic.mk_add ctx t

    let sub t = Arithmetic.mk_sub ctx t

    let mul t = Arithmetic.mk_mul ctx t

    let div t1 t2 = Arithmetic.mk_div ctx t1 t2

    let ixor t1 t2 = BitVector.mk_xor ctx t1 t2

    let mod_ t1 t2 = Arithmetic.Integer.mk_mod ctx t1 t2

    let rem t1 t2 = Arithmetic.Integer.mk_rem ctx t1 t2

    let ( ! ) x = symbol x

    let ( = ) x y = eq x y

    let ( <> ) x y = distinct [x; y]

    let ( && ) x y = and_ [x; y]

    let ( || ) x y = or_ [x; y]

    let ( <=> ) x y = iff x y

    let ( ==> ) x y = imply x y

    let ( lxor ) x y = xor x y

    let ( < ) x y = lt x y

    let ( <= ) x y = le x y

    let ( > ) x y = gt x y

    let ( >= ) x y = ge x y

    let ( + ) x y = add [x; y]

    let ( - ) x y = sub [x; y]

    let ( * ) x y = mul [x; y]

    let ( / ) x y = div x y

    let ( mod ) x y = mod_ x y

    let simplify ?params t = Expr.simplify t params

    let subst e s =
      let s1, s2 = List.unzip s in
      Expr.substitute e s1 s2

    let to_string t = Expr.to_string t

    let raw t = t

    let with_typ : type a b. (a, b) Type.t -> a -> b t =
     fun ty x ->
      match ty with
      | Int -> bigint x
      | Real -> rat x
      | Num -> rat x
      | Bool -> bool x
      | Array (_, _) -> raise @@ Error "Can't reify an array"
  end

  module Symbol = struct
    type ('a, 'b) t = ('a, 'b) Type.t * Expr.expr

    let get_typ = fst

    let rec sort : type a b. (a, b) Type.t -> Sort.sort = function
      | Int -> Arithmetic.Integer.mk_sort ctx
      | Bool -> Boolean.mk_sort ctx
      | Real -> Arithmetic.Real.mk_sort ctx
      | Num -> Arithmetic.Real.mk_sort ctx
      | Array (src, dst) -> Z3Array.mk_sort ctx (sort src) (sort dst)

    let declare (type a b) (ty : (a, b) Type.t) s : (a, b) t =
      match ty with
      | Int -> (Int, Arithmetic.Integer.mk_const_s ctx s)
      | Bool -> (Bool, Boolean.mk_const_s ctx s)
      | Real -> (Real, Arithmetic.Real.mk_const_s ctx s)
      | Num -> (Num, Arithmetic.Real.mk_const_s ctx s)
      | Array (src, dst) ->
          (Array (src, dst), Z3Array.mk_const_s ctx s (sort src) (sort dst))

    let term (type a b) (ty : (a, b) Type.t) (e : b Term.t) : (a, b) t = (ty, e)

    let trustme (type a b) (ty : (a, b) Type.t) e : (a, b) t = (ty, e)
  end

  module Z3Array = struct
    open Z3Array

    let get a i = mk_select ctx a i

    let set a i v = mk_store ctx a i v

    let make (Type.Array (src, _)) v = mk_const_array ctx (Symbol.sort src) v

    let default a = mk_term_array ctx a

    let of_array ~typ ~default arr =
      let a0 = make typ default in
      Array.fold ~f:(fun a (k, v) -> set a k v) ~init:a0 arr

    let of_indexed ~typ ~default arr =
      let a0 = make (Array (Int, typ)) default in
      let n = Array.length arr in
      let rec aux i a =
        if i < n then a else aux (i + 1) @@ set a (Term.int i) arr.(i)
      in
      aux 0 a0

    let of_list ~typ ~default arr =
      let a0 = make typ default in
      List.fold_left ~f:(fun a (k, v) -> set a k v) ~init:a0 arr
  end

  type sat =
    | Unsat of Z3.Expr.expr Lazy.t  (** Proof *)
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

  (** {2 Solver calls} *)
  module Solver = struct
    include Z3.Solver

    type t = solver

    let make () = mk_simple_solver ctx

    let add ~solver x = add solver [x]

    let pop x = pop x 1

    let check ~solver l =
      match check solver l with
      | UNSATISFIABLE -> Unsat (lazy (opt_get @@ get_proof solver))
      | UNKNOWN -> Unknown (get_reason_unknown solver)
      | SATISFIABLE -> Sat (lazy (opt_get @@ get_model solver))
  end

  (** {2 Optimizing solver calls} *)
  module Optimize = struct
    include Z3.Optimize

    type t = optimize

    let make () = mk_opt ctx

    let add ~solver x = add solver [Term.raw x]

    let add_soft ~id ~solver ~weight x = add_soft solver (Term.raw x) weight id

    let minimize ~solver x = minimize solver x

    let maximize ~solver x = maximize solver x

    let get_upper handle = Symbol.term Num (get_upper handle)

    let get_lower handle = Symbol.term Num (get_lower handle)

    let check ~solver l =
      List.iter ~f:(add ~solver) l ;
      let open Z3.Optimize in
      let v =
        match check solver with
        | Z3.Solver.UNSATISFIABLE ->
            Unsat
              ( lazy
                (failwith "get_proof is not implemented for optimizing solvers.") )
        | Z3.Solver.UNKNOWN -> Unknown (get_reason_unknown solver)
        | Z3.Solver.SATISFIABLE -> Sat (lazy (opt_get @@ get_model solver))
      in
      v
  end

  (** {2 Model extraction} *)
  module Model = struct
    exception No_value of Expr.expr

    let bool_of_lbool = function
      | Z3enums.L_TRUE -> true
      | Z3enums.L_FALSE -> false
      | Z3enums.L_UNDEF -> raise (Z3.Error "lbool")

    let get_value (type a b) ~model ((ty, t) : (a, b) Symbol.t) : a =
      let get_val t =
        match Model.eval model t true with
        | None -> raise (No_value t)
        | Some x -> x
      in
      let rec aux : type a b. (a, b) Type.t -> b Term.t -> a =
       fun ty t ->
        match ty with
        | Int -> Z.of_string @@ Arithmetic.Integer.numeral_to_string @@ get_val t
        | Bool -> bool_of_lbool @@ Boolean.get_bool_value @@ get_val t
        | Real -> Q.of_string @@ Arithmetic.Real.numeral_to_string @@ get_val t
        | Num -> Q.of_string @@ Arithmetic.Real.numeral_to_string @@ get_val t
        | Array (src, dst) ->
            let f v = aux dst (Z3Array.get t (Term.with_typ src v)) in
            f
      in
      aux ty t

    let eval (type a) ~model (term : a Term.t) : a Term.t =
      Option.value_exn (Model.eval model term true)

    let to_string = Model.to_string
  end
end
