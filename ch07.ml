type file_info = string

let dummy_file_info = ""

(* for hints *)
type arg_name = string

type term =
  (* A variable is a number - its de Bruijn index. *)
  | TmVar of file_info * int * int
  (* An abstraction carries just a subterm for the abstraction's body *)
  | TmAbs of file_info * arg_name * term
  (* An application carries the two subterms being applied *)
  | TmApp of file_info * term * term

type binding = NameBind

type context = (string * binding) list

exception NoRuleApplies

(* let rec print_term ctx term =
  match term with
  | TmAbs (fi, arg_name, t) ->
      let ctx', arg_name' = fresh ctx arg_name in
      Printf.sprintf "(lambda %s. $s)" arg_name' (print_term ctx' t)
  | TmApp (fi, t1, t2) ->
      Printf.sprintf "(%s %s)" (print_term ctx t1) (print_term ctx t2)
  | TmVar (fi, name, len) ->
      if ctxlength ctx = n then index2name fi ctx x else "bad index" *)

let shift_term distance t =
  let rec walk depth t =
    match t with
    | TmVar (fi, idx, len) ->
        if idx >= depth then TmVar (fi, idx + distance, len + distance)
        else TmVar (fi, idx, len + distance)
    | TmAbs (fi, arg_name, body) -> TmAbs (fi, arg_name, walk (depth + 1) body)
    | TmApp (fi, t1, t2) -> TmApp (fi, walk depth t1, walk depth t2)
  in
  walk 0 t

let subst_term arg s term =
  let rec walk distance t =
    match t with
    | TmVar (fi, idx, len) ->
        if idx = arg + distance then shift_term distance s
        else TmVar (fi, idx, len)
    | TmAbs (fi, arg_name, body) ->
        TmAbs (fi, arg_name, walk (distance + 1) body)
    | TmApp (fi, t1, t2) -> TmApp (fi, walk distance t1, walk distance t2)
  in
  walk 0 term

let is_val ctx t = match t with TmAbs _ -> true | _ -> false

let app s t = shift_term (-1) (subst_term 0 (shift_term 1 s) t)

let rec eval1 ctx t =
  match t with
  | TmApp (fi, TmAbs (_, arg_name, body), t2) when is_val ctx t2 -> app t2 body
  | TmApp (fi, t1, t2) when is_val ctx t1 ->
      let t2' = eval1 ctx t2 in
      TmApp (fi, t1, t2')
  | TmApp (fi, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (fi, t1', t2)
  | _ -> raise NoRuleApplies

let rec eval ctx t =
  try
    let t' = eval1 ctx t in
    eval ctx t'
  with NoRuleApplies -> t
