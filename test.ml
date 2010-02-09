module CPS =
struct
  type ident = string
  type atom =
      Atom_int of int
    | Atom_ident of ident
  type primop =
      Primop_add
    | Primop_sub
    | Primop_mul
  type condop =
      Condop_lss
    | Condop_grt
    | Condop_leq
    | Condop_geq
    | Condop_eq
    | Condop_neq
  type expression =
      Exp_primop of primop * atom * atom * ident * expression
    | Exp_condop of condop * atom * atom * expression * expression
    | Exp_app of ident * atom list
    | Exp_fix of (ident * ident list * expression) list * expression
end

module Semantics =
struct
  type value =
      Value_int of int
    | Value_func of CPS.ident list * CPS.expression
    | Value_halt

  let rec eval_primop (op:CPS.primop) (z1:int) (z2:int) =
    Value_int (match op with
                   CPS.Primop_add -> z1 + z2
                 | CPS.Primop_sub -> z1 - z2
                 | CPS.Primop_mul -> z1 * z2)
  and eval_condop (op:CPS.condop) (z1:int) (z2:int) =
    match op with
        CPS.Condop_lss -> z1 <  z2
      | CPS.Condop_grt -> z1 >  z2
      | CPS.Condop_leq -> z1 <= z2
      | CPS.Condop_geq -> z1 >= z2
      | CPS.Condop_eq  -> z1 =  z2
      | CPS.Condop_neq -> z1 <> z2

  exception Abort
  exception Unknown_identifier of CPS.ident

  type env = CPS.ident -> value
  let env0 id = raise (Unknown_identifier id)
  let rec update id v eta = fun id' -> if id = id' then v else eta id'
  let rec updaten idl vl eta =
    match idl, vl with
        id :: idl, v :: vl -> update id v (updaten idl vl eta)
      | [], [] -> eta
      | _ -> raise Abort
  let lookup id eta = eta id

  let eval_atom (a:CPS.atom) (eta:env): value =
    match a with
        CPS.Atom_int z -> Value_int z
      | CPS.Atom_ident id -> lookup id eta

  let rec eval (e:CPS.expression) (eta:env): value list =
    match e with
        CPS.Exp_primop (op, a1, a2, id, e) ->
          let v = match (eval_atom a1 eta, eval_atom a2 eta) with
              (Value_int z1, Value_int z2) -> eval_primop op z1 z2
            | _ -> raise Abort
          in eval e (update id v eta)
      | CPS.Exp_condop (op, a1, a2, e1, e2) ->
          let cond = match (eval_atom a1 eta, eval_atom a2 eta) with
              (Value_int z1, Value_int z2) -> eval_condop op z1 z2
            | _ -> raise Abort
          in eval (if cond then e1 else e2) eta
      | CPS.Exp_app (id, al) ->
          let f = match (lookup id eta) with
              Value_func (idl, e) -> (fun vl -> eval e (updaten idl vl eta))
            | Value_halt -> (fun vl -> vl)
            | _ -> raise Abort
          in f (List.map (fun a -> eval_atom a eta) al)
      | CPS.Exp_fix (fl, e) ->
          let eta = List.fold_left (fun eta (id, idl, e) -> update id (Value_func (idl, e)) eta) eta fl in
            eval e eta
end



let rec fact =
  CPS.Exp_condop (
    CPS.Condop_leq, CPS.Atom_ident "x", CPS.Atom_int 0,
    CPS.Exp_app ("k", [CPS.Atom_ident "y"]),
    CPS.Exp_primop (CPS.Primop_mul, CPS.Atom_ident "x", CPS.Atom_ident "y", "y'",
                    CPS.Exp_primop (CPS.Primop_sub, CPS.Atom_ident "x", CPS.Atom_int 1, "x'",
                                    CPS.Exp_app ("fact", [CPS.Atom_ident "x'";
                                                          CPS.Atom_ident "y'";
                                                          CPS.Atom_ident "k"]))))
and fact_sample =
  CPS.Exp_fix (
    ["fact", ["x"; "y"; "k"], fact],
    CPS.Exp_app ("fact", [CPS.Atom_ident "x";
                          CPS.Atom_int 1;
                          CPS.Atom_ident "k"]))
;;

let env = Semantics.updaten
  ["k"; "x"]
  [Semantics.Value_halt;
   Semantics.Value_int 7]
     Semantics.env0 in
  Semantics.eval fact_sample env
;;
