(* cps.mli - Interface of the CPS module *)

(* type of identifiers *)
type ident = string

(* type of atomic expressions *)
type atom =
    Atom_int of int
  | Atom_var of ident
  | Atom_label of ident

(* type of primitive operators *)
type operator =
    Op_add
  | Op_sub
  | Op_mul

(* type of CPS expressions *)
type expression =
    Exp_op of operator * atom list * ident list * expression list
  | Exp_app of atom * atom list
  | Exp_fix of (ident * ident list * expression) list * expression

