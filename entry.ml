open TypeExpr
open Corenode
open DeclNode
open SortNode
open Schedule
open WriteCode

let entry conf name n =
  let res = combineNode name n in
  match res with
  | None -> raise (Failure "No rules?\n")
  | Some (g, rules) ->
     let s = schedule rules in
     writeOut name conf g rules s
