open KNormal

let ( ||| ) o f =
  match o with
  | Some _ -> o
  | None -> f ()

(* collect (innermost) floatable binding *)
let rec collect bv ctx = function
  | IfEq (x, y, e1, e2) ->
      collect bv (fun e1' -> ctx (IfEq (x, y, e1', e2))) e1 ||| (fun () ->
        collect bv (fun e2' -> ctx (IfEq (x, y, e1, e2'))) e2)
  | IfLE (x, y, e1, e2) ->
      collect bv (fun e1' -> ctx (IfLE (x, y, e1', e2))) e1 ||| (fun () ->
        collect bv (fun e2' -> ctx (IfLE (x, y, e1, e2'))) e2)
  | Let ((x, t), e1, e2) ->
      collect bv (fun e1' -> ctx (Let ((x, t), e1', e2))) e1 ||| (fun () ->
        if S.subset (fv e1) bv && not (effect e1) then Some (x, t, e1, ctx e2)
        else collect bv (fun e2' -> ctx (Let ((x, t), e1, e2'))) e2)
  | LetRec ({ body = e1 } as fundef, e2) ->
      collect bv (fun e1' -> ctx (LetRec ({ fundef with body = e1' }, e2))) e1 ||| (fun () ->
        collect bv (fun e2' -> ctx (LetRec (fundef, e2'))) e2)
  | LetTuple (xts, y, e) ->
      collect bv (fun e' -> ctx (LetTuple (xts, y, e'))) e
  | _ -> None

(* let-floating *)
(* variable names must be unique *)
let rec f bv = function
  | IfEq (x, y, e1, e2) -> IfEq (x, y, f bv e1, f bv e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, f bv e1, f bv e2)
  | Let ((x, t), e1, e2) -> Let ((x, t), f bv e1, f (S.add x bv) e2)
  | LetRec ({ name = (x, _); args = yts; body = e1 } as fundef, e2) ->
      (match collect bv (fun e1' -> LetRec ({ fundef with body = e1' }, e2)) e1 with
       | None ->
           LetRec ({ fundef with body =
                       f (S.union bv (S.of_list (x :: List.map fst yts))) e1 },
                   f (S.add x bv) e2)
       | Some (x, t, e1, e2) -> Let ((x, t), e1, f (S.add x bv) e2))
  | LetTuple (xts, y, e) ->
      LetTuple (xts, y, f (S.union bv (S.of_list (List.map fst xts))) e)
  | e -> e

let f = f S.empty
