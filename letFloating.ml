open KNormal

(* collect floatable bindings *)
let rec collect bv e k =
  match e with
  | IfEq (x, y, e1, e2) ->
      collect bv e1 (fun bv e1' ->
        collect bv e2 (fun bv e2' ->
          k bv (IfEq (x, y, e1', e2'))))
  | IfLE (x, y, e1, e2) ->
      collect bv e1 (fun bv e1' ->
        collect bv e2 (fun bv e2' ->
          k bv (IfLE (x, y, e1', e2'))))
  | Let ((x, t), e1, e2) ->
      collect bv e1 (fun bv e1' ->
        if S.subset (fv e1') bv && not (effect e1') then
          (Format.eprintf "floating %s@." x;
           Let ((x, t), e1', collect (S.add x bv) e2 k))
        else
          collect bv e2 (fun bv e2' ->
            k bv (Let ((x, t), e1', e2'))))
  | LetRec ({ body = e1 } as fundef, e2) ->
      collect bv e1 (fun bv e1' ->
        collect bv e2 (fun bv e2' ->
          k bv (LetRec ({ fundef with body = e1' }, e2'))))
  | LetTuple (xts, y, e) ->
      collect bv e (fun bv e' ->
        k bv (LetTuple (xts, y, e')))
  | e -> k bv e

(* let-floating *)
(* variable names must be unique *)
let rec g bv e = collect bv e (fun bv -> function
  | IfEq (x, y, e1, e2) -> IfEq (x, y, g bv e1, g bv e2)
  | IfLE(x, y, e1, e2) -> IfLE (x, y, g bv e1, g bv e2)
  | Let ((x, t), e1, e2) -> Let ((x, t), g bv e1, g (S.add x bv) e2)
  | LetRec ({ name = (x, _); args = yts; body = e1 } as fundef, e2) ->
      LetRec ({ fundef with body =
                  g (S.union bv (S.of_list (x :: List.map fst yts))) e1 },
              g (S.add x bv) e2)
  | LetTuple (xts, y, e) ->
      LetTuple (xts, y, g (S.union bv (S.of_list (List.map fst xts))) e)
  | e -> e)

let f = g S.empty
