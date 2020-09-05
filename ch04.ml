type file_info = string

let dummy_file_info = ""

type term =
  | TmTrue of file_info
  | TmFalse of file_info
  | TmIf of file_info * term * term * term
  | TmZero of file_info
  | TmSucc of file_info * term
  | TmPred of file_info * term
  | TmIsZero of file_info * term

(* 考虑自然数的定义 O, SO, SSO。这里仅将 zero 和 succ 两种形式的看作 numeric value  *)
(* 而 pred 被看作 function application *)
let rec is_numeric term =
  match term with
  | TmZero _ -> true
  | TmSucc (_, t1) -> is_numeric t1
  | _ -> false

let is_val term =
  match term with
  | TmTrue _ -> true
  | TmFalse _ -> true
  | t when is_numeric t -> true
  | _ -> false

exception NoRuleApplies

(* one-step evaluation *)
let rec eval1 term =
  match term with
  | TmIf (_, TmTrue _, t, _) -> t
  | TmIf (_, TmFalse _, _, f) -> f
  | TmIf (fi, cond, t, f) ->
      let b = eval1 cond in
      TmIf (fi, b, t, f)
  | TmSucc (fi, x) ->
      let x' = eval1 x in
      TmSucc (fi, x')
  | TmPred (_, TmSucc (_, x)) when is_numeric x -> x
  | TmPred (fi, x) ->
      let x' = eval1 x in
      TmPred (fi, x')
  | TmIsZero (_, TmZero _) -> TmTrue dummy_file_info
  | TmIsZero (_, TmSucc (_, x)) when is_numeric x -> TmFalse dummy_file_info
  | TmIsZero (fi, x) ->
      let x' = eval1 x in
      TmIsZero (fi, x')
  | _ -> raise NoRuleApplies

let rec eval t =
  try
    let t' = eval1 t in
    eval t'
  with NoRuleApplies -> t
