open! Core
open! Z3

module type Context = sig
  val ctx : Z3.context
end

module Make (C : Context) = struct
  let ctx = C.ctx

  module Type = struct
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

    let rec sexp_of_t : type a b. (a, b) t -> Sexp.t = function
      | Int -> Atom "Int"
      | Real -> Atom "Real"
      | Bool -> Atom "Bool"
      | Pseudo_bool -> Atom "Pseudo_bool"
      | Tuple_2 (t, t') -> List [ Atom "Tuple_2"; sexp_of_t t; sexp_of_t t' ]
      | Array (t, t') -> List [ Atom "Array"; sexp_of_t t; sexp_of_t t' ]

    let rec to_sort : type a b. (a, b) t -> Sort.sort = function
      | Int -> Arithmetic.Integer.mk_sort ctx
      | Pseudo_bool -> Arithmetic.Integer.mk_sort ctx
      | Bool -> Boolean.mk_sort ctx
      | Real -> Arithmetic.Real.mk_sort ctx
      | Tuple_2 (t, t') ->
          let s = to_sort t in
          let s' = to_sort t' in
          Z3.Tuple.mk_sort ctx (Symbol.mk_string ctx "t2")
            [ Sort.get_name s; Sort.get_name s' ]
            [ s; s' ]
      | Array (t, t') ->
          let s = to_sort t in
          let s' = to_sort t' in
          Z3.Z3Array.mk_sort ctx s s'
  end

  module Term = struct
    type ('a, 'b) t = ('a, 'b) Type.t * Z3.Expr.expr

    let simplify ?params (t, x) = (t, Expr.simplify x params)
    let const t n = (t, Expr.mk_fresh_const ctx n (Type.to_sort t))
    let ( = ) (_, x) (_, y) = (Type.Bool, Boolean.mk_eq ctx x y)
    let to_string (_, x) = Z3.Expr.to_string x
    let raw (_, x) = x
    let raws = List.map ~f:raw
    let distinct xs = (Type.Bool, Boolean.mk_distinct ctx (raws xs))
    let cast s (_, e) = (s, e)

    let mk_quantifier is_forall t f =
      let s = Type.to_sort t in
      let n = Z3.Symbol.mk_int ctx 0 in
      let _, body = f (t, Z3.Expr.mk_const ctx n s) in
      let q =
        Z3.Quantifier.mk_quantifier ctx is_forall [ s ] [ n ] body None [] [] None
          None
        |> Z3.Quantifier.expr_of_quantifier
      in
      (Type.Bool, q)

    let forall t f = mk_quantifier true t f
    let exists t f = mk_quantifier false t f
  end

  module Status = struct
    type model = Model.model

    let sexp_of_model m = Sexp.List (Sexp.of_string_many (Model.to_string m))

    type t = Unsat | Sat of model option Lazy.t | Unknown of string
    [@@deriving sexp_of]
  end

  module Solver = struct
    open Solver

    (**
         type solver
  type status = UNSATISFIABLE | UNKNOWN | SATISFIABLE
  val string_of_status : status -> string
  val get_help : solver -> string
  val set_parameters : solver -> Params.params -> unit
  val get_param_descrs : solver -> Params.ParamDescrs.param_descrs
  val get_num_scopes : solver -> int
  val push : solver -> unit
  val pop : solver -> int -> unit
  val reset : solver -> unit
  val add : solver -> Expr.expr list -> unit
  val assert_and_track_l : solver -> Expr.expr list -> Expr.expr list -> unit
  val assert_and_track : solver -> Expr.expr -> Expr.expr -> unit
  val get_num_assertions : solver -> int
  val get_assertions : solver -> Expr.expr list
  val check : solver -> Expr.expr list -> status
  val get_model : solver -> Model.model option
  val get_proof : solver -> Expr.expr option
  val get_unsat_core : solver -> Expr.expr list
  val get_reason_unknown : solver -> string
  val get_statistics : solver -> Statistics.statistics
  val mk_solver : context -> Symbol.symbol option -> solver
  val mk_solver_s : context -> string -> solver
  val mk_simple_solver : context -> solver
  val mk_solver_t : context -> Tactic.tactic -> solver
  val translate : solver -> context -> solver
  val to_string : solver -> string
    *)

    type t = solver

    let to_string = to_string
    let sexp_of_t x = [%sexp_of: string] (to_string x)

    let create ?params () =
      match params with
      | Some ps -> mk_solver_s ctx ps
      | None -> mk_simple_solver ctx

    let push = push
    let pop ?(n = 1) s = pop s n
    let reset = reset
    let add s ts = add s (Term.raws ts)

    let check s ts =
      match check s (Term.raws ts) with
      | UNSATISFIABLE -> Status.Unsat
      | UNKNOWN -> Unknown (get_reason_unknown s)
      | SATISFIABLE -> Sat (lazy (get_model s))
  end

  module Optimizer = struct
    open Optimize

    (*
             type optimize
      type handle
      val mk_opt : context -> optimize
      val get_help : optimize -> string
      val set_parameters : optimize -> Params.params -> unit
      val get_param_descrs : optimize -> Params.ParamDescrs.param_descrs
      val add : optimize -> Expr.expr list -> unit
      val add_soft :
        optimize -> Expr.expr -> string -> Symbol.symbol -> handle
      val maximize : optimize -> Expr.expr -> handle
      val minimize : optimize -> Expr.expr -> handle
      val check : optimize -> Solver.status
      val get_model : optimize -> Model.model option
      val get_lower : handle -> Expr.expr
      val get_upper : handle -> Expr.expr
      val push : optimize -> unit
      val pop : optimize -> unit
      val get_reason_unknown : optimize -> string
      val to_string : optimize -> string
      val get_statistics : optimize -> Statistics.statistics
      val from_file : optimize -> string -> unit
      val from_string : optimize -> string -> unit
      val get_assertions : optimize -> Expr.expr list
      val get_objectives : optimize -> Expr.expr list
*)

    type t = optimize

    let create () = mk_opt ctx
    let add s ts = add s (Term.raws ts)

    let check s ts =
      add s ts;
      match check s with
      | UNSATISFIABLE -> Status.Unsat
      | UNKNOWN -> Unknown (get_reason_unknown s)
      | SATISFIABLE -> Sat (lazy (failwith "unsupported"))
  end

  module Bool = struct
    type t = (bool, [ `Bool ]) Term.t

    let const = Term.const Type.Bool
    let true_ = (Type.Bool, Boolean.mk_true ctx)
    let false_ = (Type.Bool, Boolean.mk_false ctx)
    let bool x = (Type.Bool, Boolean.mk_val ctx x)
    let not (_, x) = (Type.Bool, Boolean.mk_not ctx x)
    let ite (_, x1) (t, x2) (_, x3) = (t, Boolean.mk_ite ctx x1 x2 x3)
    let ( <=> ) (_, x1) (_, x2) = (Type.Bool, Boolean.mk_iff ctx x1 x2)
    let ( ==> ) (_, x1) (_, x2) = (Type.Bool, Boolean.mk_implies ctx x1 x2)
    let xor (_, x1) (_, x2) = (Type.Bool, Boolean.mk_xor ctx x1 x2)
    let and_ xs = (Type.Bool, Boolean.mk_and ctx (List.map xs ~f:(fun (_, x) -> x)))
    let ( && ) x y = and_ [ x; y ]
    let or_ xs = (Type.Bool, Boolean.mk_or ctx (List.map xs ~f:(fun (_, x) -> x)))
    let ( || ) x y = or_ [ x; y ]
  end

  module Num (S : sig
    type sort

    val sort : sort
  end) =
  struct
    open Arithmetic
    open S

    let ( < ) (_, x) (_, y) = (Type.Bool, mk_lt ctx x y)
    let ( <= ) (_, x) (_, y) = (Type.Bool, mk_le ctx x y)
    let ( > ) (_, x) (_, y) = (Type.Bool, mk_gt ctx x y)
    let ( >= ) (_, x) (_, y) = (Type.Bool, mk_ge ctx x y)
    let ( ** ) (_, x) (_, y) = (sort, mk_power ctx x y)
    let add xs = (sort, mk_add ctx (Term.raws xs))
    let ( + ) x y = add [ x; y ]
    let sub xs = (sort, mk_sub ctx (Term.raws xs))
    let ( - ) x y = sub [ x; y ]
    let mul xs = (sort, mk_mul ctx (Term.raws xs))
    let ( * ) x y = mul [ x; y ]
    let ( / ) (_, x) (_, y) = (sort, mk_div ctx x y)
    let ( ~- ) (_, x) = (sort, mk_unary_minus ctx x)
  end

  module Int = struct
    open Arithmetic.Integer

    type t = (Z.t, [ `Int ]) Term.t

    include Num (struct
      type sort = (Z.t, [ `Int ]) Type.t

      let sort = Type.Int
    end)

    let sort = Type.Int
    let const = Term.const Type.Int
    let int i = (sort, mk_numeral_i ctx i)
    let bigint i = (sort, mk_numeral_s ctx (Z.to_string i))
    let ( mod ) (_, x) (_, y) = (sort, mk_mod ctx x y)
    let rem (_, x) (_, y) = (sort, mk_rem ctx x y)
  end

  module Real = struct
    open Arithmetic.Real

    type t = (Q.t, [ `Real ]) Term.t

    include Num (struct
      type sort = (Q.t, [ `Real ]) Type.t

      let sort = Type.Real
    end)

    let const = Term.const Type.Real
    let to_int (_, x) = (Type.Int, mk_real2int ctx x)
    let of_int (_, x) = (Type.Real, Arithmetic.Integer.mk_int2real ctx x)
  end

  module Pseudo_bool = struct
    type t = (bool, [ `Pseudo_bool ]) Term.t

    let sort = Type.Pseudo_bool

    let const s n =
      let v = Term.const Type.Pseudo_bool n in
      Solver.add s [ Bool.(Int.(int 0 <= v) && Int.(v <= int 1)) ];
      v

    let to_int (_, e) = (Int.sort, e)
    let to_bool x = Term.(to_int x = Int.int 1)
    let of_bool x = Term.cast sort Bool.(ite x (Int.int 1) (Int.int 0))
    let true_ = Term.cast sort (Int.int 1)
    let false_ = Term.cast sort (Int.int 0)
    let and_ xs = Int.(add xs >= int (List.length xs))
    let or_ xs = Int.(add xs > int 0)
    let ( && ) x y = Int.(x + y >= int 2)
    let ( || ) x y = Int.(x + y > int 0)
    let not x = Term.cast sort Int.(int 1 - to_int x)
    let ( ==> ) x y = Int.(x <= y)
  end

  module T2 = struct
    type ('a1, 'a2, 'b1, 'b2) t = ('a1 * 'a2, [ `Tuple of 'b1 * 'b2 ]) Term.t

    let create (t1, x1) (t2, x2) =
      let t = Type.Tuple_2 (t1, t2) in
      let s = Type.to_sort t in
      let mk = Tuple.get_mk_decl s in
      (t, Expr.mk_app ctx mk [ x1; x2 ])

    let get1 (t, x) =
      let s = Type.to_sort t in
      match t with
      | Type.Tuple_2 (t1, _) -> (
          ( t1,
            match Tuple.get_field_decls s with
            | [ f; _ ] -> Expr.mk_app ctx f [ x ]
            | _ -> assert false ))
      | _ -> .

    let get2 (t, x) =
      let s = Type.to_sort t in
      match t with
      | Type.Tuple_2 (_, t2) -> (
          ( t2,
            match Tuple.get_field_decls s with
            | [ _; f ] -> Expr.mk_app ctx f [ x ]
            | _ -> assert false ))
      | _ -> .

    let of_tuple (x, x') = create x x'
    let to_tuple x = (get1 x, get2 x)
  end

  module Array = struct
    type ('a1, 'a2, 'b1, 'b2) t = ('a1 -> 'a2, [ `Array of 'b1 * 'b2 ]) Term.t

    let get (t, x) (_, x') =
      match t with
      | Type.Array (_, t'') -> (t'', Z3.Z3Array.mk_select ctx x x')
      | _ -> .

    let set (t, x) (_, x') (_, x'') = (t, Z3.Z3Array.mk_store ctx x x' x'')
  end

  module Model = struct
    type t = Z3.Model.model

    let conv_bool v =
      match Z3.Boolean.get_bool_value v with
      | Z3enums.L_TRUE -> Some true
      | Z3enums.L_FALSE -> Some false
      | Z3enums.L_UNDEF -> None

    let conv_int v = Z.of_string @@ Z3.Arithmetic.Integer.numeral_to_string v

    let conv (type a b) (t : (a, b) Type.t) v : a option =
      match t with
      | Type.Int -> Some (conv_int v)
      | Bool -> conv_bool v
      | Real -> Some (Q.of_string @@ Z3.Arithmetic.Real.numeral_to_string v)
      | Pseudo_bool ->
          let x = conv_int v in
          if Z.equal x Z.zero then Some false
          else if Z.equal x Z.one then Some true
          else None
      | _ -> failwith "unsupported"

    let eval m (t, e) = Option.bind (Z3.Model.eval m e true) ~f:(conv t)
  end
end
