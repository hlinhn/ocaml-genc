open Corenode

type sched = (int * int * ruleDt list) list
let schedule (rules : ruleDt list) : sched =
  let rec grow list (a, b) =
    match list, (a, b) with
    | [], (a, b) -> [(a, [b])]
    | ((a, bs) :: rest), (a', b) ->
       if a'== a then
         (a, b :: bs) :: rest
       else
         (a, bs) :: (grow rest (a', b)) in
  let periods = List.fold_left grow []
                               (List.map (fun x -> (x.rulePeriod, x)) rules) in
  let rec drop i ls =
    match i, ls with
    | (0, _) -> ls
    | (_, []) -> []
    | (i, ls) -> drop (i - 1) (List.tl ls) in
  
  let lub r (ls : ruleDt list list) =
    let minI = r.rulePhase in
    let min ls = List.fold_left (fun a b -> if (a < b) then a else b) (List.hd ls) ls in
    let rec lub' i ls =
      match i, ls with
      | (i, []) -> i
      | _ ->
         if (List.hd ls == (min ls)) then i
         else (lub' (i + 1) (List.tl ls)) in
    lub' minI (drop minI (List.map List.length ls)) in
  
  let take i ls = 
    let rec driverTake ls res = function
      | 0 -> res
      | n -> driverTake (List.tl ls) (res @ [List.hd ls]) (n -1) in
    driverTake ls [] i in
  
  let insertAt i r ls =
    (take i ls) @ ((r :: (List.nth ls i)) :: (drop (i + 1) ls)) in
  
  let rec combine3 f l y =
    match f, l, y with
    | [], [], [] -> []
    | fh::ft, lh::lt, yh::yt -> (fh, lh, yh) :: (combine3 ft lt yt)
    | _ -> invalid_arg "combine3" in
  
  let range a b =
    let rec rangeDriver a b ls =
      if (a >= b) then List.rev ls
      else rangeDriver (a + 1) b (a::ls) in
    rangeDriver a b [] in
  let repeat a e =
    let rec repeatDriver a e ls =
      if (a <= 0) then ls
      else repeatDriver (a - 1) e (e::ls) in
    repeatDriver a e [] in
  
  let rec placeRules ls p = function
    | [] -> List.filter (fun (_, _, r) -> r != [])
                        (combine3 (repeat p p) (range 0 p) ls)
    | r::rs -> placeRules (insertAt (lub r ls) r ls) p rs in
  
  let orderByPhase ls =
    List.sort (fun x y -> Pervasives.compare x.rulePhase y.rulePhase) ls in
  let spread (p, ls) =
    placeRules (repeat p []) p (orderByPhase ls) in
  List.flatten (List.map spread periods)
               
