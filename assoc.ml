(* flatten let-bindings (just for prettier printing) *)

open KNormal

let threshold = ref 0

let rec g e sk k = (* ネストしたletの簡約 (caml2html: assoc_g) *)
  match e with
  | IfEq (x, y, e1, e2) ->
      if sk < !threshold then IfEq (x, y, g e1 sk k, g e2 sk k)
      else k (IfEq (x, y, g e1 0 (fun e1' -> e1'), g e2 0 (fun e2' -> e2')))
  | IfLE (x, y, e1, e2) ->
      if sk < !threshold then IfLE (x, y, g e1 sk k, g e2 sk k)
      else k (IfLE (x, y, g e1 0 (fun e1' -> e1'), g e2 0 (fun e2' -> e2')))
  | Let (xt, e1, e2) ->
      let e2' = g e2 sk k in
      g e1 (1 + size e2') (fun e1' -> Let (xt, e1', e2'))
  | LetRec ({ body = e1 } as fundef, e2) ->
      LetRec ({ fundef with body = g e1 0 (fun e1' -> e1') }, g e2 sk k)
  | LetTuple (xts, y, e) -> LetTuple (xts, y, g e sk k)
  | e -> k e

let f e = g e 0 (fun e' -> e')
