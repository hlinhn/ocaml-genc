open Corenode
open TypeExpr
       
let node name design =
  addName name
  >>= fun name' -> get >>=
    fun (g1, parent) ->
    let (a, (g2, child)) = addNode { g1 with gNameList = [] } name' design in
    put ({ g2 with gNameList = g1.gNameList @ [NameH (name, g2.gNameList)] } ,
         { parent with nodeSubs = parent.nodeSubs @ [child] })
    >>= fun _ -> ret a
                   
let period p n =
  match p with
  | num when num <= 0 -> raise (Failure "Period must be greater than 0")
  | _ ->
     get >>= fun (g, a) ->
     put ({ g with gPeriod = p }, a)
     >>= fun _ -> n >>= fun r -> get
     >>= fun (g', a') -> put ({ g' with gPeriod = g.gPeriod }, a')
     >>= fun _ -> ret r
                    
let phase p n =
  get >>= fun (g, a) ->
  match p with
  | num when num >= g.gPeriod -> raise (Failure "Phase must be less than period")
  | _ ->
     put ({ g with gPhase = p }, a)
     >>= fun _ -> n >>= fun r -> get
     >>= fun (g', a) -> put ({ g' with gPhase = g.gPhase }, a)
     >>= fun _ -> ret r
                      
let var name t init =
  addName name
  >>= fun name' -> get >>=
    fun (g, cur) ->
    let c = constant t init in
    let uv' = UV (g.gVarId, name', c) in
    put ({ g with gVarId = g.gVarId + 1;
                  gNameList = g.gNameList @ [NameVar (name, c)] }, cur)
    >>= fun _ -> ret (V (uv', t))
                   
let var' : type a. string -> a typ -> a v =
  fun name t -> V ((UVExtern (name, typeTag t)), t)
    
let array name t ls =
  match ls with
  | [] -> raise (Failure "Array cannot be empty")
  | _ -> addName name
         >>= fun name' -> get >>=
           fun (g, cur) ->
           let c = List.map (constant t) ls in
           let arr = UA (g.gArrayId, name', c) in
           put ({ g with gArrayId = g.gArrayId + 1;
                         gNameList = g.gNameList @ [NameArray (name, c)] }, cur)
           >>= fun _ -> ret (A (arr, t))
                          
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
