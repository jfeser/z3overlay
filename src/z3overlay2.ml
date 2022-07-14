open! Base
open! Z3

module type Context = sig
  val ctx : Z3.context
end

module Make (C : Context) = struct
  let ctx = C.ctx

  module Type = struct
    type pint = PInt
    type preal = PReal
    type pbool = PBool
    type pstring = PString
    type pfloat = PFloat
    type ('a, 'b) parray = PArray of 'a * 'b
    type pbv = PBV of int
    type 'a pseq = PSeq of 'a
    type 'a pset = PSet of 'a

    class type ['a] penum =
      object
        method n_values : int
        method value : int -> 'a option
      end

    type ('a, 'b) t =
      | Int : (Z.t, pint) t
      | Real : (Q.t, preal) t
      | Bool : (bool, pbool) t
      | Pseudo_bool : (bool, pint) t
      | String : (string, pstring) t
      | Float : { exp_bits : int; sig_bits : int } -> (Q.t, pfloat) t
      | Array : ('a, 'x) t * ('b, 'y) t -> ('a -> 'b, ('x, 'y) parray) t
      | Bitvector : int -> (int, pbv) t
      | Set : ('a, 'b) t -> ('a -> bool, 'b pset) t

    let rec to_sort : type a b. (a, b) t -> Sort.sort = function
      | Int -> Arithmetic.Integer.mk_sort ctx
      | Pseudo_bool -> Arithmetic.Integer.mk_sort ctx
      | Bool -> Boolean.mk_sort ctx
      | Real -> Arithmetic.Real.mk_sort ctx
      | String -> Seq.mk_string_sort ctx
      | Float { exp_bits; sig_bits } -> FloatingPoint.mk_sort ctx exp_bits sig_bits
      | Bitvector n -> BitVector.mk_sort ctx n
      | Array (src, dst) -> Z3Array.mk_sort ctx (to_sort src) (to_sort dst)
      | Set elem -> Set.mk_sort ctx (to_sort elem)

    let rec unify : type a b. (a, b) t -> (a, b) t -> (a, b) t Or_error.t =
     fun t1 t2 ->
      let open Or_error.Let_syntax in
      match (t1, t2) with
      | Int, Int -> Ok Int
      | Bool, Bool -> Ok Bool
      | Pseudo_bool, Int -> Ok Int
      | Int, Pseudo_bool -> Ok Int
      | Pseudo_bool, Pseudo_bool -> Ok Pseudo_bool
      | Real, Real -> Ok Real
      | String, String -> Ok String
      | ( Float { exp_bits = e1; sig_bits = s1 },
          Float { exp_bits = e2; sig_bits = s2 } ) ->
          if s1 = s2 then
            if e1 = e2 then Ok (Float { exp_bits = e1; sig_bits = s1 })
            else
              Or_error.errorf "Floats have different exponent sizes (%d, %d)" e1 e2
          else
            Or_error.errorf "Floats have different significand sizes (%d, %d)" s1 s2
      | Bitvector n1, Bitvector n2 ->
          if n1 = n2 then Ok (Bitvector n1)
          else Or_error.errorf "Bitvectors have different widths (%d, %d)" n1 n2
      | Set e1, Set e2 ->
          let%map e = unify e1 e2 in
          Set e
      | Array (s1, d1), Array (s2, d2) ->
          let%bind s = unify s1 s2 in
          let%map d = unify d1 d2 in
          Array (s, d)

    let unify_exn t1 t2 = Or_error.ok_exn (unify t1 t2)
    let assert_unify t1 t2 = unify t1 t2 |> Or_error.ignore_m |> Or_error.ok_exn
  end

  module FuncType = struct
    type 'a pfunc = PFunc of 'a

    type ('a, 'b) t =
      | F1 : (('a, 'x) Type.t * ('b, 'y) Type.t) -> ('a -> 'b, ('x * 'y) pfunc) t
      | F2 :
          (('a, 'x) Type.t * ('b, 'y) Type.t * ('c, 'z) Type.t)
          -> ('a -> 'b -> 'c, ('x * 'y * 'z) pfunc) t
      | F3 :
          (('a, 'x) Type.t * ('b, 'y) Type.t * ('c, 'z) Type.t * ('d, 'w) Type.t)
          -> ('a -> 'b -> 'c -> 'd, ('x * 'y * 'z * 'w) pfunc) t
  end

  module Term = struct
    type ('a, 'b) t = ('a, 'b) Type.t * Z3.Expr.expr

    let simplify ?params (t, x) = (t, Expr.simplify x params)
    let const t n = (t, Expr.mk_const_s ctx n (Type.to_sort t))

    let ( = ) (t1, x) (t2, y) =
      Type.assert_unify t1 t2;
      (Type.Bool, Boolean.mk_eq ctx x y)

    let to_string (_, x) = Z3.Expr.to_string x
    let raw (_, x) = x
    let raws = List.map ~f:raw
    let distinct xs = (Type.Bool, Boolean.mk_distinct ctx (raws xs))
    let cast s (_, e) = (s, e)
  end

  module Status = struct
    type t = Unsat | Sat of Model.model option Lazy.t | Unknown of string
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

    module Handle = struct
      type 'a t = 'a * handle
    end

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
    type t = (bool, Type.pbool) Term.t

    let const = Term.const Type.Bool
    let true_ = (Type.Bool, Boolean.mk_true ctx)
    let false_ = (Type.Bool, Boolean.mk_false ctx)
    let bool x = (Type.Bool, Boolean.mk_val ctx x)
    let not (_, x) = (Type.Bool, Boolean.mk_not ctx x)

    let ite (_, x1) (t2, x2) (t3, x3) =
      (Type.unify_exn t2 t3, Boolean.mk_ite ctx x1 x2 x3)

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

    type t = (Z.t, Type.pint) Term.t

    include Num (struct
      type sort = (Z.t, Type.pint) Type.t

      let sort = Type.Int
    end)

    let sort = Type.Int
    let const = Term.const Type.Int
    let int i = (sort, mk_numeral_i ctx i)
    let bigint i = (sort, mk_numeral_s ctx (Z.to_string i))
    let ( mod ) (_, x) (_, y) = (sort, mk_mod ctx x y)
    let rem (_, x) (_, y) = (sort, mk_rem ctx x y)
    let to_real (_, x) = (Type.Real, mk_int2real ctx x)
    let to_bitvector (_, x) n = (Type.Bitvector n, mk_int2bv ctx n x)
  end

  module Real = struct
    open Arithmetic.Real

    type t = (Q.t, Type.preal) Term.t

    include Num (struct
      type sort = (Q.t, Type.preal) Type.t

      let sort = Type.Real
    end)

    let const = Term.const Type.Real
    let to_int (_, x) = (Type.Int, mk_real2int ctx x)
  end

  module Pseudo_bool = struct
    type t = (bool, Type.pint) Term.t

    let sort = Type.Pseudo_bool

    let const s n =
      let v = Term.const Type.Pseudo_bool n in
      Solver.add s [ Bool.(Int.(int 0 <= v) && Int.(v <= int 1)) ];
      v

    let to_int (_, e) = (Int.sort, e)
    let to_bool x = Term.(to_int x = Int.int 1)
    let of_bool x = Term.cast sort Bool.(ite x (Int.int 1) (Int.int 0))
    let and_ xs = Int.(add xs >= int (List.length xs))
    let or_ xs = Int.(add xs > int 0)
    let ( && ) x y = Int.(x + y >= int 2)
    let ( || ) x y = Int.(x + y > int 0)
    let not x = Term.cast sort Int.(int 1 - to_int x)
    let ( ==> ) x y = Int.(x <= y)
  end

  module Array = struct
    open Z3Array

    type ('a, 'b, 'c, 'd) t = ('a -> 'b, ('c, 'd) Type.parray) Term.t

    let const t1 t2 = Term.const (Type.Array (t1, t2))

    let get (t, x1) (tsrc, x2) =
      let (Type.Array (tsrc', tdst)) = t in
      Type.assert_unify tsrc tsrc';
      (tdst, mk_select ctx x1 x2)

    let set (t, a) (tsrc, i) (tdst, v) =
      let (Type.Array (tsrc', tdst')) = t in
      Type.assert_unify tsrc tsrc';
      Type.assert_unify tdst tdst';
      (t, mk_store ctx a i v)

    let make (Type.Array (src, _)) v = mk_const_array ctx (Type.to_sort src) v
  end

  module Func = struct
    type 'a t = 'a * Z3.FuncDecl.func_decl

    let fun_1 name t1 t2 =
      ( FuncType.F1 (t1, t2),
        FuncDecl.mk_func_decl_s ctx name [ Type.to_sort t1 ] (Type.to_sort t2) )

    let fun_2 name t1 t2 t3 =
      ( FuncType.F2 (t1, t2, t3),
        FuncDecl.mk_func_decl_s ctx name
          [ Type.to_sort t1; Type.to_sort t2 ]
          (Type.to_sort t3) )

    let fun_3 name t1 t2 t3 t4 =
      ( FuncType.F3 (t1, t2, t3, t4),
        FuncDecl.mk_func_decl_s ctx name
          [ Type.to_sort t1; Type.to_sort t2; Type.to_sort t3 ]
          (Type.to_sort t4) )
  end
end
