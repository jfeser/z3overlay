(* open! Base
 * open! Z3
 * 
 * module type Context = sig
 *   val ctx : Z3.context
 * end
 * 
 * module Make (C : Context) = struct
 *   let ctx = C.ctx
 * 
 *   module Type = struct
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
 *         method value : int -> 'a option
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
 *     type zbv = (int, pbv) t
 * 
 *     type ('a, 'b) zset = ('a -> bool, 'b pset) t
 * 
 *     let rec to_sort : type a b. (a, b) t -> Sort.sort = function
 *       | Int -> Arithmetic.Integer.mk_sort ctx
 *       | Bool -> Boolean.mk_sort ctx
 *       | Real -> Arithmetic.Real.mk_sort ctx
 *       | String -> Seq.mk_string_sort ctx
 *       | Float {exp_bits; sig_bits} -> FloatingPoint.mk_sort ctx exp_bits sig_bits
 *       | Bitvector n -> BitVector.mk_sort ctx n
 *       | Array (src, dst) -> Z3Array.mk_sort ctx (to_sort src) (to_sort dst)
 *       | Set elem -> Set.mk_sort ctx (to_sort elem)
 * 
 *     let rec unify : type a b. (a, b) t -> (a, b) t -> (a, b) t Or_error.t =
 *      fun t1 t2 ->
 *       let open Or_error.Let_syntax in
 *       match (t1, t2) with
 *       | Int, Int -> Ok Int
 *       | Bool, Bool -> Ok Bool
 *       | Real, Real -> Ok Real
 *       | String, String -> Ok String
 *       | Float {exp_bits= e1; sig_bits= s1}, Float {exp_bits= e2; sig_bits= s2} ->
 *           if s1 = s2 then
 *             if e1 = e2 then Ok (Float {exp_bits= e1; sig_bits= s1})
 *             else
 *               Or_error.errorf "Floats have different exponent sizes (%d, %d)" e1 e2
 *           else
 *             Or_error.errorf "Floats have different significand sizes (%d, %d)" s1 s2
 *       | Bitvector n1, Bitvector n2 ->
 *           if n1 = n2 then Ok (Bitvector n1)
 *           else Or_error.errorf "Bitvectors have different widths (%d, %d)" n1 n2
 *       | Set e1, Set e2 ->
 *           let%map e = unify e1 e2 in
 *           Set e
 *       | Array (s1, d1), Array (s2, d2) ->
 *           let%bind s = unify s1 s2 in
 *           let%map d = unify d1 d2 in
 *           Array (s, d)
 * 
 *     let unify_exn t1 t2 = Or_error.ok_exn (unify t1 t2)
 * 
 *     let assert_unify t1 t2 = unify t1 t2 |> Or_error.ignore |> Or_error.ok_exn
 *   end
 * 
 *   module FuncType = struct
 *     type 'a pfunc = PFunc of 'a
 * 
 *     type ('a, 'b) t =
 *       | F1 : (('a, 'x) Type.t * ('b, 'y) Type.t) -> ('a -> 'b, ('x * 'y) pfunc) t
 *       | F2 :
 *           (('a, 'x) Type.t * ('b, 'y) Type.t * ('c, 'z) Type.t)
 *           -> ('a -> 'b -> 'c, ('x * 'y * 'z) pfunc) t
 *       | F3 :
 *           (('a, 'x) Type.t * ('b, 'y) Type.t * ('c, 'z) Type.t * ('d, 'w) Type.t)
 *           -> ('a -> 'b -> 'c -> 'd, ('x * 'y * 'z * 'w) pfunc) t
 *   end
 * 
 *   module Term = struct
 *     type ('a, 'b) t = ('a, 'b) Type.t * Z3.Expr.expr
 * 
 *     let drop_types = List.map ~f:(fun (_, x) -> x)
 * 
 *     let simplify ?params (t, x) = (t, Expr.simplify x params)
 * 
 *     let const t n = (t, Expr.mk_const_s ctx n (Type.to_sort t))
 * 
 *     let ( = ) (t1, x) (t2, y) =
 *       Type.assert_unify t1 t2 ;
 *       (Type.Bool, Boolean.mk_eq ctx x y)
 * 
 *     let distinct xs = (Type.Bool, Boolean.mk_distinct ctx (drop_types xs))
 * 
 *     let to_string (_, x) = Z3.Expr.to_string x
 *   end
 * 
 *   module Bool = struct
 *     type t = (bool, Type.pbool) Term.t
 * 
 *     let const = Term.const Type.Bool
 * 
 *     let true_ = (Type.Bool, Boolean.mk_true ctx)
 * 
 *     let false_ = (Type.Bool, Boolean.mk_false ctx)
 * 
 *     let bool x = (Type.Bool, Boolean.mk_val ctx x)
 * 
 *     let not (_, x) = (Type.Bool, Boolean.mk_not ctx x)
 * 
 *     let ite (_, x1) (t2, x2) (t3, x3) =
 *       (Type.unify_exn t2 t3, Boolean.mk_ite ctx x1 x2 x3)
 * 
 *     let ( <=> ) (_, x1) (_, x2) = (Type.Bool, Boolean.mk_iff ctx x1 x2)
 * 
 *     let ( ==> ) (_, x1) (_, x2) = (Type.Bool, Boolean.mk_implies ctx x1 x2)
 * 
 *     let xor (_, x1) (_, x2) = (Type.Bool, Boolean.mk_xor ctx x1 x2)
 * 
 *     let and_ xs = (Type.Bool, Boolean.mk_and ctx (List.map xs ~f:(fun (_, x) -> x)))
 * 
 *     let ( && ) x y = and_ [x; y]
 * 
 *     let or_ xs = (Type.Bool, Boolean.mk_or ctx (List.map xs ~f:(fun (_, x) -> x)))
 * 
 *     let ( || ) x y = or_ [x; y]
 *   end
 * 
 *   module Num (S : sig
 *     type sort
 * 
 *     val sort : sort
 *   end) =
 *   struct
 *     open Arithmetic
 *     open S
 * 
 *     let ( < ) (_, x) (_, y) = (Type.Bool, mk_lt ctx x y)
 * 
 *     let ( <= ) (_, x) (_, y) = (Type.Bool, mk_le ctx x y)
 * 
 *     let ( > ) (_, x) (_, y) = (Type.Bool, mk_gt ctx x y)
 * 
 *     let ( >= ) (_, x) (_, y) = (Type.Bool, mk_ge ctx x y)
 * 
 *     let ( ** ) (_, x) (_, y) = (sort, mk_power ctx x y)
 * 
 *     let add xs = (sort, mk_add ctx (Term.drop_types xs))
 * 
 *     let ( + ) x y = add [x; y]
 * 
 *     let sub xs = (sort, mk_sub ctx (Term.drop_types xs))
 * 
 *     let ( - ) x y = sub [x; y]
 * 
 *     let mul xs = (sort, mk_mul ctx (Term.drop_types xs))
 * 
 *     let ( * ) x y = mul [x; y]
 * 
 *     let ( / ) (_, x) (_, y) = (sort, mk_div ctx x y)
 * 
 *     let ( ~- ) (_, x) = (sort, mk_unary_minus ctx x)
 *   end
 * 
 *   module Int = struct
 *     open Arithmetic.Integer
 * 
 *     type t = (Z.t, Type.pint) Term.t
 * 
 *     include Num (struct
 *       type sort = (Z.t, Type.pint) Type.t
 * 
 *       let sort = Type.Int
 *     end)
 * 
 *     let sort = Type.Int
 * 
 *     let const = Term.const Type.Int
 * 
 *     let int i = (sort, mk_numeral_i ctx i)
 * 
 *     let bigint i = (sort, mk_numeral_s ctx (Z.to_string i))
 * 
 *     let ( mod ) (_, x) (_, y) = (sort, mk_mod ctx x y)
 * 
 *     let rem (_, x) (_, y) = (sort, mk_rem ctx x y)
 * 
 *     let to_real (_, x) = (Type.Real, mk_int2real ctx x)
 * 
 *     let to_bitvector (_, x) n = (Type.Bitvector n, mk_int2bv ctx n x)
 *   end
 * 
 *   module Real = struct
 *     open Arithmetic.Real
 * 
 *     type t = (Q.t, Type.preal) Term.t
 * 
 *     include Num (struct
 *       type sort = (Q.t, Type.preal) Type.t
 * 
 *       let sort = Type.Real
 *     end)
 * 
 *     let const = Term.const Type.Real
 * 
 *     let to_int (_, x) = (Type.Int, mk_real2int ctx x)
 *   end
 * 
 *   module Array = struct
 *     open Z3Array
 * 
 *     type ('a, 'b, 'c, 'd) t = ('a -> 'b, ('c, 'd) Type.parray) Term.t
 * 
 *     let const t1 t2 = Term.const (Type.Array (t1, t2))
 * 
 *     let get (t, x1) (tsrc, x2) =
 *       let (Type.Array (tsrc', tdst)) = t in
 *       Type.assert_unify tsrc tsrc' ;
 *       (tdst, mk_select ctx x1 x2)
 * 
 *     let set (t, a) (tsrc, i) (tdst, v) =
 *       let (Type.Array (tsrc', tdst')) = t in
 *       Type.assert_unify tsrc tsrc' ;
 *       Type.assert_unify tdst tdst' ;
 *       (t, mk_store ctx a i v)
 * 
 *     let make (Type.Array (src, _)) v = mk_const_array ctx (Type.to_sort src) v
 * 
 *     let default (t, a) = mk_term_array ctx a
 * 
 *     let of_array ~typ ~default arr =
 *       let a0 = make typ default in
 *       Array.fold ~f:(fun a (k, v) -> set a k v) ~init:a0 arr
 * 
 *     let of_indexed ~typ ~default arr =
 *       let a0 = make (Array (Int, typ)) default in
 *       let n = Array.length arr in
 *       let rec aux i a =
 *         if i < n then a else aux (i + 1) @@ set a (Term.int i) arr.(i)
 *       in
 *       aux 0 a0
 * 
 *     let of_list ~typ ~default arr =
 *       let a0 = make typ default in
 *       List.fold_left ~f:(fun a (k, v) -> set a k v) ~init:a0 arr
 *   end
 * 
 *   module Func = struct
 *     type 'a t = 'a * Z3.FuncDecl.func_decl
 * 
 *     let fun_1 name t1 t2 =
 *       ( FuncType.F1 (t1, t2)
 *       , FuncDecl.mk_func_decl_s ctx name [Type.to_sort t1] (Type.to_sort t2) )
 * 
 *     let fun_2 name t1 t2 t3 =
 *       ( FuncType.F2 (t1, t2, t3)
 *       , FuncDecl.mk_func_decl_s ctx name
 *           [Type.to_sort t1; Type.to_sort t2]
 *           (Type.to_sort t3) )
 * 
 *     let fun_3 name t1 t2 t3 t4 =
 *       ( FuncType.F3 (t1, t2, t3, t4)
 *       , FuncDecl.mk_func_decl_s ctx name
 *           [Type.to_sort t1; Type.to_sort t2; Type.to_sort t3]
 *           (Type.to_sort t4) )
 *   end
 * end *)
