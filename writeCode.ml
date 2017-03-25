open TypeExpr
open Corenode
open SortNode
                 
type config = { stateName : string;
                funcName : string;
                arduino : bool;
                cCode : (string * string);
                inout : pinMode list;
              }
                
let defaultConf = { stateName = "state";
                    funcName = "";
                    arduino = false;
                    cCode = ("", "");
                    inout = [];
                  }
                    
let globalClk = "__global_clock"
                  
let cType : etype -> string = function
  | Bool -> "bool" | Int -> "int"
  | Int64 -> "int64_t" | Int32 -> "int32_t" | Int16 -> "int16_t" | Int8 -> "int8_t"
  | UInt64 -> "uint64_t" | UInt32 -> "uint32_t" | UInt16 -> "uint16_t" | UInt8 -> "uint8_t"
  | Float -> "float"
               
let print_const c =
  let open Stdint in
  match c with
  | CBool true -> "true"
  | CBool false -> "false"
  | CInt a -> Pervasives.string_of_int a
  | CInt64 a -> Int64.to_string a
  | CInt32 a -> Int32.to_string a
  | CInt16 a -> Int16.to_string a
  | CInt8 a -> Int8.to_string a
  | CWord64 a -> Uint64.to_string a
  | CWord32 a -> Uint32.to_string a
  | CWord16 a -> Uint16.to_string a
  | CWord8 a -> Uint8.to_string a
  | CFloat a -> String.concat "" [Pervasives.string_of_float a; "F"]
                              
let declarePins ls =
  let dPin p =
    match p with
    | DIn n | AIn n -> Printf.sprintf "   pinMode(%d, INPUT);" n
    | DOut n | AOut n -> Printf.sprintf "   pinMode(%d, OUTPUT);" n in
  String.concat "\n" (List.map dPin ls)

let setupPins ls =
  String.concat "\n" ["void setup() {"; declarePins ls; "}"]
                
let declName def cNames =
  let rec checkEmpty = function
    | NameH (_, i) -> if (i == []) then true
                      else List.for_all (fun x -> (checkEmpty x)) i
    | NameVar (_, _) -> false
    | NameArray (_, _) -> false in
  
  let rec decl i = function
    | NameH (n, items) ->
       String.concat "" ( (Printf.sprintf "%sstruct { /* %s */\n" i n)
                          :: List.map (fun x -> decl (Printf.sprintf "  %s" i) x) items
                          @ [Printf.sprintf "%s} %s;\n" i n])
    | NameVar (n, c) -> Printf.sprintf "%s%s %s;\n" i (cType (typeOf typeConst c)) n
    | NameArray (n, c) -> Printf.sprintf "%s%s %s[%d];\n" i (cType (typeOf typeConst (List.hd c))) n (List.length c) in
  
  let rec declVal i = function
    | NameH (n, items) ->
       String.concat "" ( (Printf.sprintf "%s{ /* %s */\n" i n)
                          :: [(String.concat ",\n" (List.map (declVal (Printf.sprintf "  %s" i)) items))]
                          @ ["\n"; i; "}"])
    | NameVar (n, c) -> Printf.sprintf "%s/* %s */ %s" i n (print_const c)
    | NameArray (n, c) -> 
       String.concat "" [ (Printf.sprintf "%s/* %s */\n%s{ " i n i);
                          String.concat (Printf.sprintf "\n%s, " i)
                                        (List.map print_const c);
                          "\n"; i; "}"] in
  
  if checkEmpty cNames then ""
  else
    let str = decl "" cNames in
    let substr = String.sub str 0 ((String.length str) - 2) in
    String.concat "" [if def then "" else "extern "; substr;
                      if def then (Printf.sprintf " =\n %s" (declVal "" cNames))
                      else ""; ";\n"]

let checkExtern fuv ls id =
  match fuv with
  | UV (_, _, _) | UVArray (UA (_, _, _), _) -> ""
  | UVArray (UAExtern (un, ut), index) ->
     let nameType e n t =
       match e with
       | UVArray (UAExtern (ni, ti), _) -> 
          ni = n && ti == t
       | _ -> false in
     let nls = List.filter (fun x -> nameType x un ut) ls in
     if List.length nls < 1
     then let () = Printf.printf "Warning: %s might not be declared\n" un in ""
     else if List.length nls > 1
     then let () = Printf.printf "Warning: %s might be declared too many times\n" un in ""
     else let declared = List.hd nls in
          let str = 
            match declared with
            | UVArray (UAExtern (_, _), UConst (CInt num)) ->
               if num < 0 then ""
               else Printf.sprintf "assert(%s < %d);\n" (id index) num
            | _ -> let () = Printf.printf "Parsing problem in check\n" in "" in
          str            
  | UVExtern (n, _) ->
     if List.mem fuv ls then ""
     else let () = Printf.printf "Warning: %s might not be declared\n" n in ""
                 
let get_ind = function
  | Some a -> a
  | None -> ""
              
let codeUE conf exls ues s (ue_, n) =
  let ops = List.map (fun x -> if List.mem_assoc x ues then List.assoc x ues else "") (ueUpstream ue_) in
  let basic =
    let a = if ops != [] then Some (List.hd ops) else None in
    let b = if List.length ops >= 2 then Some (List.nth ops 1) else None in
    let c = if List.length ops > 2 then Some (List.nth ops 2) else None in
    let a' = get_ind a in
    let b' = get_ind b in
    let c' = get_ind c in
    
    match ue_ with
    | UVRef (UV (_, n, _)) -> [conf.stateName; "."; n]
    | UVRef (UVArray (UA (_, k, _), _)) -> [conf.stateName; "."; k; "["; a'; "]"]
    | UVRef (UVArray (UAExtern (k, _), _)) -> [k; "["; a'; "]"]
    | UVRef (UVExtern (n, _)) -> [n]
    | UConst cn -> [print_const cn]                         
    | UAdd (_, _) -> [a'; " + "; b']
    | USub (_, _) -> [a'; " - "; b']
    | UMul (_, _) -> [a'; " * "; b']
    | UDiv (_, _) -> [a'; " / "; b']
    | UMod (_, _) -> [a'; " % "; b']
    | UNot _ -> ["! "; a']
    | UAnd _ -> [String.concat " && " ops]
    | UEq (_, _) -> [a'; " == "; b']
    | ULt (_, _) -> [a'; " < "; b']
    | UMux (_, _, _) -> [a'; " ? "; b'; " : "; c']
    | UPow (_, _) -> ["pow"; "(" ; a'; ", "; b'; ")"] in
  
  let checkUe expr = match expr with
    | UVRef var -> checkExtern var exls (fun x -> let a = if ops != [] then Some (List.hd ops) else None in get_ind a)
    | _ -> "" in

  String.concat "" [checkUe ue_; s; cType (typeOf typeUE ue_); " "; n; " = "; String.concat "" basic; ";\n"]
                
let codeRule conf rule =
  let open ParseDecl in
  let ues = sortNode (allUEs rule) in
  let id ue' = List.assoc ue' ues in
  let listExtern = createDecl (fst conf.cCode) in
  let codeAction (f, args) = String.concat "" ["   "; f (List.map id args); ";\n"] in
  
  let varName fuv =
    match fuv with
    | UV (_, n, _) -> String.concat "." [conf.stateName; n]
    | UVArray (UA (_, n, _), index) -> Printf.sprintf "%s.%s[%s]" conf.stateName n (id index)
    | UVArray (UAExtern (n, _), index) -> Printf.sprintf "%s[%s]" n (id index)
    | UVExtern (n, _) -> n in
                                                                                
  let codeInout dir ls declared =
    let filtered =
      let getIndir x = let p = fst x in
                       match p with
                       | AIn _ | DIn _ -> true
                       | AOut _ | DOut _ -> false in
      if dir == `In
      then List.filter (getIndir) ls
      else List.filter (fun x -> not (getIndir x)) ls in

    let read_from (pin, v) =
      match pin with 
      | DIn n -> Printf.sprintf "%s = digitalRead(%d);" (varName v) n
      | AIn n -> Printf.sprintf "%s = analogRead(%d);" (varName v) n
      | _ -> "" in
    
    let write_to (pin, v) =
      match pin with
      | DOut n -> Printf.sprintf "digitalWrite(%d, %s);" n (varName v)
      | AOut n -> Printf.sprintf "analogWrite(%d, %s);" n (varName v)
      | _ -> "" in
    
    if dir == `In
    then String.concat "\n" (List.map (fun x -> if List.mem (fst x) declared then read_from x else failwith "Undeclared input pin") filtered)
    else String.concat "\n" (List.map (fun x -> if List.mem (fst x) declared then write_to x else failwith "Undeclared output pin") filtered) in
        
  let codeAssign exls (fuv, tue) =
    let lh = varName fuv in
    let check = checkExtern fuv exls (fun x -> id x) in
    let checkrh = match tue with
      | UVRef fv -> checkExtern fv exls (fun x -> id x)
      | _ -> "" in
    Printf.sprintf "%s%s   %s = %s;\n" check checkrh lh (id tue) in
  
  String.concat "" ((Printf.sprintf "static void __r%d() {\n" rule.ruleId)
                    :: List.map (fun x -> codeUE conf listExtern ues "  " x) ues
                    @ ["  if ("; id rule.ruleEn ; ") { \n"]
                    @ (List.map codeAction rule.ruleActions)
                    @ [if conf.arduino then codeInout `In rule.ruleInout conf.inout else ""; "  }\n"]
                    @ (List.map (fun x -> codeAssign listExtern x) rule.ruleAssigns)
                    @ [if conf.arduino then codeInout `Out rule.ruleInout conf.inout else ""; "\n"; "}\n\n"])
                
let codeTime (p, ph, rules) =
  let callRule r = Printf.sprintf "    __r%d();" r.ruleId in
  let clockType : etype = UInt32 in
  String.concat "\n"
                [" {";
                 Printf.sprintf "   static %s __scheduling_clock = %i;" (cType clockType) ph;
                 Printf.sprintf "   if (__scheduling_clock == 0) {";
                 String.concat "\n" (List.map callRule rules);
                 Printf.sprintf "    __scheduling_clock = %i;" (p - 1);
                 "    } else {";
                 "      __scheduling_clock = __scheduling_clock - 1;";
                 "   }";
                 "}"]

                
let writeOut name conf cNames rules sched =
  let ard = if conf.arduino then setupPins conf.inout
            else "" in
  let extra = "#include <stdint.h>\n#include <assert.h>" in
  let inclArd = if conf.arduino then "#define NDEBUG" else "" in
  let c =
    String.concat "\n" (extra :: inclArd :: (fst conf.cCode)
                        :: (String.concat " " ["static"; cType UInt64; globalClk; "=0;"])
                        :: (declName true (NameH (conf.stateName, [cNames])))
                        :: ard
                        :: (List.map (fun x -> codeRule conf x) rules)
                        @ [(Printf.sprintf "void %s() {" name)]
                        @ (List.map (fun x -> codeTime x) sched)
                        @ [String.concat " " [" "; globalClk; "="; globalClk; "+ 1;"]]
                        @ ["}"] @ [snd conf.cCode]) in
  
  let h = String.concat "\n" [(declName false (NameH (conf.stateName, [cNames])))] in
  
  let filecName = String.concat "." [name; "c"] in
  let filehName = String.concat "." [name; "h"] in
  let filec = open_out filecName in
  let fileh = open_out filehName in
  
  Printf.fprintf filec "%s\n" c;
  Printf.fprintf fileh "%s\n" h;
  close_out filec;
  close_out fileh        
