type ty = TyArr of ty * ty | TyBool

type binding = NameBind | VarBind of ty

module Ctx = struct
  type t = (string * binding) list

  let emtpy = []

  let length ctx = List.length ctx

  let add_binding ctx x binding = (x, binding) :: ctx

  let add_name ctx x = add_binding ctx x NameBind

  let get_binding fi ctx i =
    try
      let _, bind = List.nth ctx i in
      bind
    with Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
      in
      error fi (msg i (List.length ctx))

  let get_type fi ctx i =
    match getbinding fi ctx i with
    | VarBind tyT -> tyT
    | _ ->
        error fi
          ( "Ctx.get_type: Wrong kind of binding for variable "
          ^ index2name fi ctx i )

  let rec is_bound ctx x =
    match ctx with
    | [] -> false
    | (y, _) :: rest -> if y = x then true else is_bound rest x
end

(* --- *)

type file_info = string

let dummy_file_info = ""

(* for hints *)
type arg_name = string

type term =
  | TmVar of file_info * int * int
  | TmAbs of file_info * string * ty * term
  | TmApp of file_info * term * term
  | TmTrue of file_info
  | TmFalse of file_info
  | TmIf of file_info * term * term * term

let rec typeof ctx t =
  match t with
  | TmVar (fi, name, _) -> Ctx.get_type fi ctx name
  | TmAbs (fi, x, tyT1, t2) ->
      let ctx' = Ctx.get_binding ctx x (VarBind tyT1) in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)
  | TmApp (fi, t1, t2) -> (
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      match tyT1 with
      | TyArr (tyT11, tyT12) ->
          if tyT2 = tyT11 then tyT12 else error fi "parameter type mismatch"
      | _ -> error fi "arrow type expected" )
  | TmTrue fi -> TyBool
  | TmFalse fi -> TyBool
  | TmIf (fi, t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if tyT2 = typeof ctx t3 then tyT2
        else error fi "arms of conditional have different types"
      else error fi "guard of conditional not a boolean"
