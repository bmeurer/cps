(* anf.mli - Interface of the Anf module *)

type constant =
    Const_int of int
  | Const_bool of bool

type operator =
    Op_add | Op_sub | Op_mul
  | Op_lss | Op_grt | Op_leq | Op_geq | Op_eq

type variable = string

type primitive =
    Prim_const of constant
  | Prim_var of variable

type expression =
    Exp_prim of primitive
  | Exp_op of primitive * operator * primitive
  | Exp_abstr of abstraction
  | Exp_app of primitive * primitive list
  | Exp_cond of primitive * expression * expression
  | Exp_let of variable * expression * expression
  | Exp_letrec of (variable * abstraction) list * expression
and abstraction =
    variable list * expression
