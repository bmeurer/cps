(* anf0.ml - Implementation of the Anf0 module *)

type constant =
    Const_int of int
  | Const_bool of bool

type operator =
    Op_add | Op_sub | Op_mul
  | Op_lss | Op_grt | Op_leq | Op_geq | Op_eq

type primitive =
    Prim_const of constant
  | Prim_var of int
  | Prim_label of int * int (* context offset (static), stack relative offset (dynamic) *)
and expression =
    Exp_prim of primitive
  | Exp_op of primitive * operator * primitive
  | Exp_abstr of abstraction
  | Exp_app of primitive * primitive list
  | Exp_cond of primitive * expression * expression
  | Exp_let of expression * expression
  | Exp_letrec of abstraction list * expression
and abstraction =
    expression
