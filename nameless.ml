(* for hints *)
type arg_name = string

type term =
  (* A variable is a number - its de Bruijn index. *)
  | TmVar of int
  (* An abstraction carries just a subterm for the abstraction's body *)
  | TmAbs of arg_name * term
  (* An application carries the two subterms being applied *)
  | TmApp of term * term

type binding = NameBind

type context = (string * binding) list

(* 每当 `term` 被代入到一个 abs 的 body 内，都需要将其所有自由变量移动一位 *)
let shift term =
  (* 根据与当前深度判断是否是自由变量 *)
  let rec h depth t =
    match t with
    (* 如果变量的编号大于等于当前深度，说明 its binder is outside of `term` *)
    (* 那么对于 `term` 来说它是一个自由变量，否则不是 *)
    | TmVar n -> TmVar (if n >= depth then n + 1 else n)
    (* 当前深度 + 1 *)
    | TmAbs (arg_name, body) -> TmAbs (arg_name, h (depth + 1) body)
    | TmApp (t1, t2) -> TmApp (h depth t1, h depth t2)
  in
  (* it starts off at 0 (meaning all vars should be shifted) *)
  h 0 term

let rec substitute from to' term =
  match term with
  (* 如果刚好是目标变量，那么替换 *)
  | TmVar n -> if from = n then to' else TmVar n
  (* 因为更深入了一层，所以需要 `shift` 一次 `to'` *)
  (* 因为更深入了一层，所以目标变量离 its binder 的距离也增加了 1 *)
  | TmAbs (arg_name, body) ->
      TmAbs (arg_name, substitute (from + 1) (shift to') body)
  | TmApp (t1, t2) -> TmApp (substitute from to' t1, substitute from to' t2)

let unshift term =
  let rec h depth t =
    match t with
    | TmVar n -> TmVar (if n >= depth then n - 1 else n)
    | TmAbs (arg_name, body) -> TmAbs (arg_name, h (depth + 1) body)
    | TmApp (t1, t2) -> TmApp (h depth t1, h depth t2)
  in
  h 0 term

let app (TmApp (TmAbs (_, body), t2)) =
  (* 这里 `t2` 进入了更深的一层，所以需要 `shift` 一次 *)
  (* 当 reducing redex，function body 的深度减小了一，则其中所有的自由变量需要负向移动一位，
     我们可以通过与 `shift` 相同的方式定义一个 `unshift` *)
  unshift (substitute 0 (shift t2) body)
