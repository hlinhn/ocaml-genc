open TypeExpr
open Corenode
open SortNode
       
type config = { stateName : string;
                funcName : string;
                cCode : (string * string);
                inout : (string * string);
              }
let defaultConf = { stateName = "state";
                    funcName = "";
                    cCode = ("", "");
                    inout = ("", "");
                  }
let globalClk = "__global_clock"
let cType : etype -> string = function
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
               
let print_const = function
  | CBool true -> "true"
  | CBool false -> "false"
  | CInt a -> Pervasives.string_of_int a
  | CFloat a -> String.concat "" [Pervasives.string_of_float a; "F"]
                              
let declName def cNames =
  let rec checkEmpty = function
    | NameH (_, i) -> if (i == []) then true
                      else
                        List.for_all (fun x -> (checkEmpty x)) i
    | NameVar (_, _) -> false
    | NameArray (_, _) -> false in
  
  let rec decl i = function
    | NameH (n, items) ->
       String.concat ""
                     ( [i;"struct { /* "; n; " */\n"] @
                         List.map
                           (fun x ->
                             decl
                               (String.concat "" ["  "; i]) x) items @ [i; "} "; n; ";\n"])
    | NameVar (n, c) -> String.concat "" [i; cType (typeOf typeConst c); " "; n; ";\n"]
    | NameArray (n, c) -> String.concat "" [i; cType (typeOf typeConst (List.hd c)); " "; n; "["; Pervasives.string_of_int (List.length c); "];\n"] in
  
  let rec declVal i = function
    | NameH (n, items) ->
       String.concat ""
                     ( [i; "{  /* "; n; " */\n"]
                       @ [(String.concat ",\n" (List.map (declVal (String.concat "" ["  "; i])) items))]
                       @ ["\n"; i; "}"])
    | NameVar (n, c) -> String.concat "" [i; "/* "; n; " */ "; print_const c]
    | NameArray (n, c) -> String.concat "" [i; "/* "; n; " */\n"; i; "{ ";
                                            String.concat (String.concat "" ["\n"; i; ", "]) (List.map print_const c);
                                            "\n"; i; "}"] in
  if checkEmpty cNames then ""
  else
    let str = decl "" cNames in
    let substr = String.sub str 0 ((String.length str) - 2) in
    String.concat "" [if def then "" else "extern ";
                      substr;
                      if def then (String.concat "" [" =\n";
                                                     declVal "" cNames])
                      else "";
                      ";\n"]
                  
                  
let get_ind = function
  | Some a -> a
  | None -> ""
              
let codeUE conf ues s (ue_, n) =
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
  
  String.concat "" [s; cType (typeOf typeUE ue_); " "; n; " = "; String.concat "" basic; ";\n"]
                
let codeRule conf rule =
  (* some precode, short of codeAction *)
  let ues = sortNode (allUEs rule) in

  let id ue' = List.assoc ue' ues in
  let codeAction (f, args) = String.concat "" ["   ";
                                               f (List.map id args);
                                               ";\n"] in
  let codeAssign (fuv, tue) =
    let lh = match fuv with
      | UV (_, n, _) -> String.concat "." [conf.stateName; n]
      | UVArray (UA (_, n, _), index) -> String.concat "" [conf.stateName; "."; n; "["; id index; "]"]
      | UVArray (UAExtern (n, _), index) -> String.concat "" [n; "["; id index; "]"]
      | UVExtern (n, _) -> n in
    String.concat "" ["  "; lh; " = "; id tue; ";\n"] in
  
  String.concat "" (["static void __r";
                     Pervasives.string_of_int rule.ruleId;
                     "() { \n"] @                        
                      List.map (fun x -> codeUE conf ues " " x) ues @
                        ["  if ("; id rule.ruleEn ; ") { \n"] @
                          [String.concat "" (List.map codeAction rule.ruleActions)] @
                            ["  }\n"] @
                              [String.concat "" (List.map (fun x -> codeAssign x) rule.ruleAssigns)] @
                                ["}\n\n"])
                
                
let codeTime (p, ph, rules) =
  let callRule r = String.concat "" ["    "; "__r";
                                     Pervasives.string_of_int r.ruleId;
                                     "();"] in
  let clockType : etype = Int in
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
  let c = String.concat "\n"
                        ((fst conf.cCode) ::
                           (String.concat " " ["static"; cType Int; globalClk; "=0;"]) ::
                             (declName true (NameH (conf.stateName, [cNames]))) ::
                               (List.map (fun x -> codeRule conf x) rules) @       
                           [String.concat "" ["void "; name; "()"; "{"]] @
                             (List.map (fun x -> codeTime x) sched) @
                               [String.concat " " [" "; globalClk; "="; globalClk; "+ 1;"]] @
                                 ["}"] @ [snd conf.cCode])
  in
  let h = String.concat "\n" [(declName false (NameH (conf.stateName, [cNames])))] in
  
  let filecName = String.concat "." [name; "c"] in
  let filehName = String.concat "." [name; "h"] in
  let filec = open_out filecName in
  let fileh = open_out filehName in
  
  Printf.fprintf filec "%s\n" c;
  Printf.fprintf fileh "%s\n" h;
  close_out filec;
  close_out fileh        
