open Corenode
open TypeExpr
       
let node name design =
  do_
  ; name' <-- addName name
  ; (g1, parent) <-- get
  ; let (a, (g2, child)) = addNode { g1 with gNameList = [] } name' design in
    put ({ g2 with gNameList = g1.gNameList @ [NameH (name, g2.gNameList)] } ,
         { parent with nodeSubs = parent.nodeSubs @ [child] })
    ; return a
             
let period p n =
  let _ = assert ( p > 0) in
  (do_
  ; (g, a) <-- get
  ; put ({ g with gPeriod = p }, a)
  ; r <-- n
  ; (g', a') <-- get
  ; put ({ g' with gPeriod = g.gPeriod }, a')
  ; return r)
                    
let phase p n =
  do_
  ; (g, a) <-- get
  ; if p >= g.gPeriod then raise (Failure "Phase must be less than period")
    else
      (do_
      ; put ({ g with gPhase = p }, a)
      ; r <-- n
      ; (g', a') <-- get
      ; put ({ g' with gPhase = g.gPhase }, a')
      ; return r)
                      
let var name t init =
  do_
  ; name' <-- addName name
  ; (g, cur) <-- get
  ; let c = constant t init in
    let uv' = UV (g.gVarId, name', c) in
    put ({ g with gVarId = g.gVarId + 1;
                  gNameList = g.gNameList @ [NameVar (name, c)] }, cur)
    ; return (V (uv', t))
                   
let var' : type a. string -> a typ -> a v =
  fun name t -> V ((UVExtern (name, typeTag t)), t)
    
let array name t ls =
  match ls with
  | [] -> raise (Failure "Array cannot be empty")
  | _ ->
     do_
    ; name' <-- addName name
    ; (g, cur) <-- get
    ; let c = List.map (constant t) ls in
      let arr = UA (g.gArrayId, name', c) in
      put ({ g with gArrayId = g.gArrayId + 1;
                    gNameList = g.gNameList @ [NameArray (name, c)] }, cur)
      ; return (A (arr, t))
                          
let array' : type a. string -> a typ -> a tArray =
  fun name t -> A ((UAExtern (name, typeTag t)), t)
    
let cond en =
  get >>= fun (g, n) ->
  put (g, { n with nodeEn = uand n.nodeEn (uecv en) })
        
let assign : type a. a v -> a e -> unit node =
  fun v e -> get >>= fun (g, n) ->
             put (g, { n with nodeAssigns = (uvcv v, uecv e) :: n.nodeAssigns })
        
let action f ues =
  get >>= fun (g, n) ->
  put (g, { n with nodeActions = n.nodeActions @ [(f, ues)] })

let (<==) v e = assign v e

let readPin p elem =
  match p with
  | AOut _ | DOut _ -> failwith "Expecting input pin"
  | _ -> get >>= fun (g, n) ->
         put (g, { n with nodeInout = n.nodeInout @ [(p, elem)] })

let writePin p elem =
  match p with
  | AIn _ | DIn _ -> failwith "Expecting output pin"
  | _ -> get >>= fun (g, n) ->
         put (g, { n with nodeInout = n.nodeInout @ [(p, elem)] })
  
