#load "anf.cmo"
#load "anf0.cmo"
#load "parser.cmo"
#load "lexer.cmo"

let string_to_expr (t:string): Anf.expression =
  Parser.expr Lexer.token (Lexing.from_string t)
;;

module Translator =
struct
  include Anf

  exception Unknown_variable of variable

  type name =
      Name_var of variable
    | Name_label of variable

  let rec translate_const (c:constant): Anf0.constant =
    match c with
        Const_int z -> Anf0.Const_int z
      | Const_bool b -> Anf0.Const_bool b
  and translate_op (op:operator): Anf0.operator =
    match op with
        Op_add -> Anf0.Op_add
      | Op_sub -> Anf0.Op_sub
      | Op_mul -> Anf0.Op_mul
      | Op_lss -> Anf0.Op_lss
      | Op_grt -> Anf0.Op_grt
      | Op_leq -> Anf0.Op_leq
      | Op_geq -> Anf0.Op_geq
      | Op_eq -> Anf0.Op_eq
  and translate_var (x:variable) (nl:name list): Anf0.primitive =
    let rec translate_var_relative nl voffset loffset =
      match nl with
          [] -> raise (Unknown_variable x)
        | (Name_var x') :: nl' ->
            if x = x' then Anf0.Prim_var voffset
            else translate_var_relative nl' (voffset + 1) loffset
        | (Name_label x') :: nl' ->
            if x = x' then Anf0.Prim_label (loffset, voffset)
            else translate_var_relative nl' voffset (loffset + 1)
    in translate_var_relative nl 0 0
  and translate_prim (p:primitive) (nl:name list): Anf0.primitive =
    match p with
        Prim_const c -> Anf0.Prim_const (translate_const c)
      | Prim_var x -> translate_var x nl
  and translate_abstr ((xl, e):abstraction) (nl:name list): Anf0.abstraction =
    translate_exp e (List.rev_append (List.map (fun x -> Name_var x) xl) nl)
  and translate_exp (e:expression) (nl:name list): Anf0.expression =
    match e with
        Exp_prim p -> Anf0.Exp_prim (translate_prim p nl)
      | Exp_op (p1, op, p2) -> Anf0.Exp_op (translate_prim p1 nl, translate_op op, translate_prim p2 nl)
      | Exp_abstr a -> Anf0.Exp_abstr (translate_abstr a nl)
      | Exp_app (p, pl) -> Anf0.Exp_app (translate_prim p nl, List.map (fun p -> translate_prim p nl) pl)
      | Exp_cond (p, e1, e2) -> Anf0.Exp_cond (translate_prim p nl, translate_exp e1 nl, translate_exp e2 nl)
      | Exp_let (x, e1, e2) -> Anf0.Exp_let (translate_exp e1 nl, translate_exp e2 ((Name_var x) :: nl))
      | Exp_letrec (xal, e) ->
          let (xl, al) = List.split xal in
          let nl = List.rev_append (List.map (fun x -> Name_label x) xl) nl in
            Anf0.Exp_letrec (List.map (fun a -> translate_abstr a nl) al, translate_exp e nl)

  let translate (e:expression): Anf0.expression =
    translate_exp e []
end


module Semantics0 =
struct
  include Anf0

  type value =
      Val_const of constant
    | Val_closure of abstraction * int
  and stack =
      (value * int) list
  and context =
      abstraction list

  let rec update s vl n =
    match vl with
        [] -> s
      | v :: vl -> update ((v, n) :: s) vl (1 + List.length s)

  let rec remove s n =
    if n = 0 then s
    else let _ :: s = s in remove s (n - 1)

  exception Abort of string
  exception Abort_prim of (primitive * stack) list

  let substack (s:stack) (n:int): stack =
    let rec substack_helper s n =
      if n = 0 then []
      else let d :: s = s in
        d :: (substack_helper s (n - 1))
    in List.rev (substack_helper (List.rev s) n)

  let rec eval_prim (p:primitive) (s:stack) (c:context): value =
    try 
      match p, s with
          Prim_const c, _ -> Val_const c
        | Prim_var 0, (v, _) :: _ -> v
        | Prim_var i, (_, n) :: s -> eval_prim (Prim_var (i - 1)) (substack s n) c
        | Prim_label (o, 0), s -> Val_closure (List.nth c o, List.length s)
        | Prim_label (o, i), (_, n) :: s -> eval_prim (Prim_label (o, i - 1)) (substack s n) c
        | _ -> raise (Abort_prim [(p, s)])
    with Abort_prim l -> raise (Abort_prim ((p, s) :: l))
  and eval_op (op:operator) (p1:primitive) (p2:primitive) (s:stack) (c:context): value =
    (match (eval_prim p1 s c, eval_prim p2 s c) with
         Val_const (Const_int z1), Val_const (Const_int z2) ->
           Val_const (match op with
                          Op_add -> Const_int (z1 + z2)
                        | Op_sub -> Const_int (z1 - z2)
                        | Op_mul -> Const_int (z1 * z2)
                        | Op_lss -> Const_bool (z1 < z2)
                        | Op_grt -> Const_bool (z1 > z2)
                        | Op_leq -> Const_bool (z1 <= z2)
                        | Op_geq -> Const_bool (z1 >= z2)
                        | Op_eq -> Const_bool (z1 = z2))
       | _ -> raise (Abort "Cannot apply op to anything but ints"))
  and eval_exp (e:expression) (s:stack) (c:context): value =
    match e with
        Exp_prim p ->
          eval_prim p s c
      | Exp_op (p1, op, p2) ->
          eval_op op p1 p2 s c
      | Exp_abstr a ->
          Val_closure (a, List.length s)
      | Exp_app (p1, pl) ->
          (match (eval_prim p1 s c, List.map (fun p -> eval_prim p s c) pl) with
               Val_closure (a, n), vl ->
                 eval_exp a (update s vl n) c
             | _ -> raise (Abort "Left side of app must be a closure"))
      | Exp_cond (p, e1, e2) ->
          (match (eval_prim p s c) with
               Val_const (Const_bool true) -> eval_exp e1 s c
             | Val_const (Const_bool false) -> eval_exp e2 s c
             | _ -> raise (Abort "Condition for if then else must be true or false"))
      | Exp_let (e1, e2) ->
          eval_exp e2 ((eval_exp e1 s c, List.length s) :: s) c
      | Exp_letrec (al, e) ->
          eval_exp e s (List.rev_append al c)

  let eval (e:expression): value =
    eval_exp e [] []


                    (* Reg     Expr/Offset Stack         Stack   Recs *)
  type configuration = value * (expression * int) list * stack * context
  let eval_iter_step (conf:configuration): configuration =
    match conf with
        (_, (Exp_prim p, 0) :: el, s, c) ->
          (eval_prim p s c, el, s, c)
      | (_, (Exp_op (p1, op, p2), 0) :: el, s, c) ->
          (eval_op op p1 p2 s c, el, s, c)
      | (_, (Exp_abstr a, 0) :: el, s, c) ->
          (Val_closure (a, List.length s), el, s, c)
      | (r, (Exp_app (p1, pl), 0) :: el, s, c) ->
          (match (eval_prim p1 s c, List.map (fun p -> eval_prim p s c) pl) with
               Val_closure (a, n), vl ->
                 (r, (a, 0) :: (Exp_app (p1, pl), 1) :: el, (update s vl n), c)
             | _ -> raise (Abort "Left side of app must be a closure"))
      | (r, (Exp_app (_, pl), 1) :: el, s, c) ->
          (r, el, remove s (List.length pl), c)
      | (r, (Exp_cond (p, e1, e2), 0) :: el, s, c) ->
          (match (eval_prim p s c) with
               Val_const (Const_bool true) -> (r, (e1, 0) :: el, s, c)
             | Val_const (Const_bool false) -> (r, (e2, 0) :: el, s, c)
             | _ -> raise (Abort "Condition for if then else must be true or false"))
      | (r, (Exp_let (e1, e2), 0) :: el, s, c) ->
          (r, (e1, 0) :: (Exp_let (e1, e2), 1) :: el, s, c)
      | (r, (Exp_let (e1, e2), 1) :: el, s, c) ->
          (r, (e2, 0) :: (Exp_let (e1, e2), 2) :: el, update s [r] (List.length s), c)
      | (r, (Exp_let (e1, e2), 2) :: el, s, c) ->
          (r, el, remove s 1, c)
      | (r, (Exp_letrec (al, e), 0) :: el, s, c) ->
          (r, (e, 0) :: (Exp_letrec (al, e), 1) :: el, s, List.rev_append al c)
      | (r, (Exp_letrec (al, e), 1) :: el, s, c) ->
          (r, el, s, remove c (List.length al))
      | _ -> raise (Abort "Invalid configuration")
  let eval_iter (e:expression) =
    let finished conf =
      match conf with
          (_, [], _, _) -> true
        | _ -> false
    and result (v, _, _, _) =
      v
    in let cr = ref (Val_const (Const_int 0), [e, 0], [], []) in
      while not (finished (!cr)) do
        cr := (eval_iter_step !cr)
      done;
      result (!cr)
end


let fact_s = "
let rec fact =
  fun (x) ->
    let t = x = 0 in
      if t then 1
      else
        let z = x - 1 in
          let y = fact (z) in
            x * y
in fact (6)
";;

let fact_e =
  string_to_expr fact_s
;;

let fact_e0 =
  Translator.translate fact_e
;;

Semantics0.eval fact_e0;;
Semantics0.eval_iter fact_e0;;


let sum_s = ("let rec sum = fun (f, a, b) ->
  let t = a > b in
    if t then 0
    else
      let a1 = a + 1 in
        let s = sum (f, a1, b) in
          let fa = f (a) in
            fa + s
and sqr = fun (x) -> x * x
in sum (sqr, 1, 3)");;

let sum_e =
  string_to_expr sum_s
;;

let sum_e0 =
  Translator.translate sum_e
;;

Semantics0.eval sum_e0;;
Semantics0.eval_iter sum_e0;;
