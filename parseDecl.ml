open Str
open TypeExpr
       
let searchPair ?nested:(n = true) str s e ~ssize:ss ~esize:es =
  let rec buildPair nested ols cls =
    match List.length ols, List.length cls with
  | (0, 0) -> []
  | (0, _) -> failwith "Unmatched closing"
  | (_, 0) -> failwith "Unmatched opening"
  | (_, _) ->
     let c = List.hd cls in
     let (s, rst) = List.partition (fun x -> x < c) ols in
     if List.length s == 0 then failwith "Unmatched closing"
     else 
       if nested then
         let revs = List.rev s in
         (List.hd revs, c)
         :: buildPair nested ((List.rev (List.tl revs)) @ rst) (List.tl cls)
       else (List.hd s, c) :: buildPair nested rst (List.tl cls) in
       
  let rec allpos str reg ~pfrom:sp ~size:s= 
    let stp = try Str.search_forward reg str sp with _ -> -1 in
    if stp != -1 then stp :: (allpos str reg ~pfrom:(stp + s) ~size:s)
    else [] in
  let popens = allpos str s ~pfrom:0 ~size:ss in
  let pcloses = allpos str e ~pfrom:0 ~size:es in
  let pairs = buildPair n popens pcloses in
  List.map (fun (x, y) -> (x, y + es)) pairs
  
let splitUp str ls =
  let makeChain ls = (List.flatten (List.map (fun (x, y) -> [x; y]) ls)) in
  match ls with
  | [] -> [str]
  | _ -> let chain = makeChain ls in
         let combined = List.combine (0 :: chain) (chain @ [String.length str]) in
         List.map (fun (x, y) -> String.trim (String.sub str x (y - x))) combined
                  
let rec rmMiddle ls ind =
  match ls with
  | [] -> []
  | _ ->
     if ind then (List.hd ls) :: (rmMiddle (List.tl ls) (not ind))
     else (rmMiddle (List.tl ls) (not ind))
         
let removeComment str =
  let ls = searchPair ~nested:false str (Str.regexp "/\\*") (Str.regexp "\\*/") ~ssize:2 ~esize:2 in
  let strls = splitUp str ls in
  String.concat "" (rmMiddle strls true)
                
let searchForLine str dir ~pos:p =
  let edp =
    if dir == `Forward
    then try Str.search_forward (Str.regexp "\n") str p with _ -> -1
    else try Str.search_backward (Str.regexp "\n") str p with _ -> -1 in
  if edp == -1 then if dir == `Forward then String.length str else 0
  else if dir == `Forward then edp else edp + 1
                     
let rec removeOneLine str expr =
  let stp = try Str.search_forward expr str 0 with _ -> -1 in
  if stp == -1 then str
  else let edp = try Str.search_forward (Str.regexp "\n") str stp with _ -> -1 in
       if edp == -1 then String.sub str 0 stp
       else removeOneLine (String.concat "" [String.sub str 0 stp;
                                             String.sub str (edp + 1) (String.length str - edp - 1)]) expr

let rec rm_nested ls =
  match ls with
  | [] -> []
  | _ -> if List.exists (fun (x, _) -> x < fst (List.hd ls)) ls
         then rm_nested (List.tl ls)
         else List.hd ls :: rm_nested (List.tl ls) 
                          
let removeFunction str =
  let ls = searchPair ~nested:true str (Str.regexp "{") (Str.regexp "}") ~ssize:1 ~esize:1 in
  let rm = rm_nested ls in
  let rec declFun ls ind pre =
    match ls with
    | [] -> if String.length pre > 0 then [pre] else []
    | _ ->
       if ind then (String.concat "" [pre; List.hd ls])
                   :: (declFun (List.tl ls) (not ind) "")
       else
         let spl = (Str.split (Str.regexp ";") (List.hd ls)) in
         if List.length spl == 0 then declFun (List.tl ls) (not ind) ""
         else
           let revspl = List.rev spl in
           (List.rev (List.tl revspl))
           @ (declFun (List.tl ls) (not ind) (List.hd revspl)) in
  
  let split_to = declFun (splitUp str rm) false "" in
  let isDecl str = (not (String.contains str ';'))
                   && ((String.contains str '=')
                       || (not (String.contains str '('))) in
  List.filter isDecl split_to
  
let parseDecl str =
  let noMultiCmt = removeComment (String.trim str) in
  let res_ = removeOneLine noMultiCmt (Str.regexp "#") in
  let res = removeOneLine res_ (Str.regexp "//") in
  let decl = removeFunction res in
  let arrayDecl str = String.contains str '[' || String.contains str '{'
                      || String.contains str '*' in
  List.partition arrayDecl decl

let oType : string -> etype = fun str ->
  let open TypeExpr in
  match str with
  | "bool" -> Bool
  | "int64_t" -> Int64 | "int32_t" -> Int32 | "int16_t" -> Int16
  | "int8_t" -> Int8 | "int" -> Int
  | "uint64_t" -> UInt64 | "uint32_t" -> UInt32 | "uint16_t" -> UInt16
  | "uint8_t" -> UInt8 | "char" -> UInt8
  | "float" -> Float
  | _ -> failwith "Unknown type"
                 
let findType str =
  let collection = ["bool"; "int"; "float"; "int64_t"; "int32_t"; "int16_t"; "int8_t"; "uint64_t"; "uint32_t"; "uint16_t"; "uint8_t"; "char"] in
  let inviw = Str.split (Str.regexp "[ \t\n]+") (String.trim str) in
  List.filter (fun s -> List.mem s inviw) collection

let getType str =
  let t_ls = findType str in
  if List.length t_ls > 1 then failwith "Two different types in the same line?"
  else if List.length t_ls == 0 then None
  else Some (oType (List.hd t_ls))
            
let getName str =
  let ind = try String.index str '=' with _ -> -1 in
  let name = if ind == -1 then str else String.sub str 0 ind in
  let ls = Str.split (Str.regexp "[ \t\n]+") (String.trim name) in
  List.hd (List.rev ls)
          
let varDecl str =
  let typ = getType str in
  match typ with
  | None -> None
  | Some t -> let name = getName str in
              Some (UVExtern (name, t))

let arrDecl str =
  let mtyp = getType str in
  match mtyp with
  | None -> None
  | Some t -> let name = getName str in
              if String.contains name '[' then
                let s = String.index name '[' in
                let e = String.index name ']' in
                let real = if String.contains name '*'
                           then let () = Printf.printf "Warning: might not compile in C" in
                                let ind = (String.index name '*') in
                                String.sub name ind (s - ind)
                           else String.sub name 0 s in
                let size = if s + 1 < e then int_of_string (String.sub name (s + 1) (e - s - 1)) else -1 in
                Some (UVArray (UAExtern (real, t), UConst (CInt size)))
              else None
  
let rec getExtern ls =
  match ls with
  | [] -> []
  | _ -> match List.hd ls with
         | None -> getExtern (List.tl ls)
         | Some x -> x :: getExtern (List.tl ls)
                                    
let createDecl str =
  let (arr, onev) = parseDecl str in
  getExtern (List.map varDecl onev @ List.map arrDecl arr)
