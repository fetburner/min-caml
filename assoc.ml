(* flatten let-bindings (just for prettier printing) *)

open KNormal

let rec g e k = (* ネストしたletの簡約 (caml2html: assoc_g) *)
  match e with
  | IfEq (x, y, e1, e2) -> IfEq (x, y, g e1 k, g e2 k)
  | IfLE (x, y, e1, e2) -> IfLE (x, y, g e1 k, g e2 k)
  | Let (xt, e1, e2) -> g e1 (fun e1' -> Let (xt, e1', g e2 k))
  | LetRec ({ body = e1 } as fundef, e2) ->
      LetRec ({ fundef with body = g e1 (fun e1' -> e1') }, g e2 k)
  | LetTuple (xts, y, e) -> LetTuple (xts, y, g e k)
  | e -> k e

let f e = g e (fun e' -> e')
