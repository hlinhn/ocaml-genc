(*
1. variables assignment
2. operations
3. conditions to execute
4. time period
5. sub nodes which gets further expanded 
 *)
open TypeExpr
type name = string
type namelist = NameH of (name * namelist list)
              | NameVar of (name * TypeExpr.const)
              | NameArray of (name * TypeExpr.const list)
                               
type globalSt = { gRuleId:int;
                  gVarId :int;
                  gArrayId : int;
                  gNameList :namelist list;
                  gPeriod:int;
                  gPhase :int
                }
type pinMode =
  | DIn of int
  | DOut of int
  | AIn of int
  | AOut of int
                  
type nodeDt = { nodeId     :int;
                nodeName   :name;
                nodeNames  :name list;
                nodeEn     :ue;
                nodeSubs   :nodeDt list;
                nodePeriod :int;
                nodePhase  :int;
                nodeAssigns:(uv * ue) list;
                nodeActions:((string list -> string) * ue list) list;
                nodeInout : (pinMode * uv) list
              }
                
type ruleDt = { ruleId     :int;
                ruleName   :name;
                ruleEn     :ue;
                ruleAssigns:(uv * ue) list;
                ruleActions:((string list -> string) * ue list) list;
                ruleInout  :(pinMode * uv) list;
                rulePeriod :int;
                rulePhase  :int
              }
                
type nodeSt = (globalSt * nodeDt)
type 'a node = Node of (nodeSt -> ('a * nodeSt))
                         
let get = Node (fun s -> (s, s))
let put (s : nodeSt) = Node (fun _ -> ((), s))
let ret a = Node (fun s -> (a, s))
let (>>=) (Node m) f = Node (fun s ->
                           let (a, s') = m s in
                           let (Node m') = f a in
                           m' s')
let checkName name =
  if (String.length name) < 1 then raise (Failure "Name can't be empty")
  else
    let isAlpha c =
      let code = Char.code c in
      (code >= Char.code('A') && code <= Char.code('Z')) ||
        (code >= Char.code('a') && code <= Char.code('z')) in
    let isNumeric c =
      let code = Char.code c in
      (code >= Char.code('0') && code <= Char.code('9')) in
    let properStart c = (isAlpha c) || (c == '_') in
    if (properStart (String.get name 0)) then
      (String.iter (fun x -> if (isAlpha x || isNumeric x ||
                                   (x == '_')) then ()
                             else raise (Failure "Not a valid identifier")) name)
    else raise (Failure "Not a valid identifier")
               
let addName name =
  get >>=
    fun (g, cur) ->
    let () = checkName name in
    if List.mem name cur.nodeNames then raise (Failure "Name not unique in node")
    else
      put (g, { cur with nodeNames = name :: cur.nodeNames} ) >>=
        fun _ ->
        ret (String.concat "." [cur.nodeName; name])
            
let addNode gstt name (Node a) =
  a ({ gstt with gRuleId = gstt.gRuleId + 1 },
     { nodeId = gstt.gRuleId;
       nodeName = name;
       nodeNames = [];
       nodeEn = ubool true;
       nodeSubs = [];
       nodePeriod = gstt.gPeriod;
       nodePhase = gstt.gPhase;
       nodeAssigns = [];
       nodeActions = [];
       nodeInout = [] }
    )

let rec createRules en nodedt =
  let isRule = not (nodedt.nodeAssigns = []
                    && nodedt.nodeActions = []
                    && nodedt.nodeInout = []) in
  let () = match isRule with
    | true -> Printf.printf "%s: Period: %d Phase: %d\n" nodedt.nodeName nodedt.nodePeriod nodedt.nodePhase
    | false -> Printf.printf "%s: Empty\n" nodedt.nodeName in
  
  let enable = uand en nodedt.nodeEn in
  let enAssigns (uv', ue') = (uv', umux enable ue' (UVRef uv')) in
  let rule = { ruleId = nodedt.nodeId;
               ruleName = nodedt.nodeName;
               ruleEn = enable;
               ruleActions = nodedt.nodeActions;
               ruleAssigns = List.map enAssigns nodedt.nodeAssigns;
               ruleInout = nodedt.nodeInout;
               rulePeriod = nodedt.nodePeriod;
               rulePhase = nodedt.nodePhase
             } in
  let rules = (List.flatten (List.map (fun x -> createRules enable x) nodedt.nodeSubs)) in
  if isRule then rule :: rules else rules
                                      
let rec trimNames ls =
  let f = function
    | NameH (_, []) -> false
    | _ -> true in
  
  match ls with
  | NameH (n, items) -> NameH (n, List.filter f (List.map trimNames items))
  | _ -> ls
           
let combineNode name node =
  let (_, (g, nodedt)) = addNode
                           { gRuleId = 0;
                             gVarId = 0;
                             gArrayId = 0;
                             gNameList = [];
                             gPeriod = 1;
                             gPhase = 0 } name node in
  let rules = createRules (ubool true) nodedt in

  if rules = [] then let () = Printf.printf "No rules\n" in None 
  else Some (trimNames (NameH (name, g.gNameList)), rules)
            
let allUEs r =
  r.ruleEn ::
    (List.map (fun x -> snd x) r.ruleAssigns)
  @ (List.flatten (snd (List.split r.ruleActions)))                     
      
