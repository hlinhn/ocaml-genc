open Stdint
       
type etype =
  | Bool | Float | Int64 | Int32 | Int16 | Int8 | Int
  | UInt64 | UInt32 | UInt16 | UInt8
                   
type const =
  | CBool of bool | CInt of int
  | CInt64 of int64 | CInt32 of int32 | CInt16 of int16 | CInt8 of int8
  | CWord64 of uint64 | CWord32 of uint32 | CWord16 of uint16 | CWord8 of uint8
  | CFloat of float

module type TYPE = sig
  type t
  val typeOfM : t -> etype
end
type 'a type_impl = (module TYPE with type t = 'a)                      
                      
let typeOf : 'a type_impl -> 'a -> etype =                                        fun (type a) (find_type : a type_impl) (x : a) ->
  let module Type = (val find_type : TYPE with type t = a) in
  Type.typeOfM x
               
module Type_const :
TYPE with type t = const = struct
                 type t = const
                 let typeOfM : const -> etype = function
                   | CBool _ -> Bool | CInt _ -> Int
                   | CInt64 _ -> Int64 | CInt32 _ -> Int32 | CInt16 _ -> Int16
                   | CInt8 _ -> Int8 | CWord64 _ -> UInt64 | CWord32 _ -> UInt32
                   | CWord16 _ -> UInt16 | CWord8 _ -> UInt8
                   | CFloat _ -> Float
               end
let typeConst : const type_impl =
  (module Type_const : TYPE with type t = const)

    
type _ typ =
  | Int64 : int64 typ | Int32 : int32 typ | Int16 : int16 typ | Int8 : int8 typ
  | UInt64 : uint64 typ | UInt32 : uint32 typ | UInt16 : uint16 typ | UInt8 : uint8 typ
  | Bool : bool typ | Int : int typ
  | Float : float typ
                  
let typeTag : type a. a typ -> etype =
  fun t ->
  match t with
  | Int64 -> Int64 | Int32 -> Int32 | Int16 -> Int16 | Int8 -> Int8
  | UInt64 -> UInt64 | UInt32 -> UInt32 | UInt16 -> UInt16 | UInt8 -> UInt8 
  | Float -> Float | Int -> Int
  | Bool -> Bool 
              
type ua =
  | UA of (int * string * const list)
  | UAExtern of (string * etype)
                  
type 'a tArray = A : ua * 'a typ -> 'a tArray
                                       
type uv =
  | UV of (int * string * const)
  | UVArray of (ua * ue)
  | UVExtern of (string * etype)
                  
 and ue =
   | UVRef of uv
   | UConst of const
   | UAdd of (ue * ue)
   | USub of (ue * ue)
   | UMul of (ue * ue)
   | UDiv of (ue * ue)
   | UMod of (ue * ue)
   | UNot of ue
   | UAnd of ue list
   | UEq of (ue * ue)
   | ULt of (ue * ue)
   | UMux of (ue * ue * ue)
   | UPow of (ue * ue)
               
type 'a v = V : uv * 'a typ -> 'a v
                                  
type 'a e =
  | VRef : 'a v -> 'a e
  | Const : 'a typ * 'a -> 'a e
  | Add : 'a e * 'a e -> 'a e
  | Sub : 'a e * 'a e -> 'a e
  | Mul : 'a e * 'a e -> 'a e
  | Div : 'a e * 'a e -> 'a e
  | Mod : 'a e * 'a e -> 'a e
  | Not : bool e -> bool e
  | And : bool e * bool e -> bool e
  | Eq : 'a e * 'a e -> bool e
  | Lt : 'a e * 'a e -> bool e
  | Mux : bool e * 'a e * 'a e -> 'a e
  | Pow : float e * float e -> float e
                                     
module Type_ua :
TYPE with type t = ua = struct
                 type t = ua
                 let typeOfM : ua -> etype = function
                   | UA (_, _, ls) -> typeOf typeConst (List.hd ls)
                   | UAExtern (_, t) -> t
               end
let typeUa : ua type_impl =                                       
  (module Type_ua : TYPE with type t = ua)                                
    
module Type_uv :
TYPE with type t = uv = struct
                 type t = uv                                
                 let typeOfM = function
                   | UV (_, _, a) -> typeOf typeConst a
                   | UVArray (a, _) -> typeOf typeUa a
                   | UVExtern (_, a) -> a
               end             
let typeUV : uv type_impl =
  (module Type_uv : TYPE with type t = uv)
    
module Type_ue :
TYPE with type t = ue = struct
                 type t = ue
                 let rec typeOfM = function
                   | UVRef u -> typeOf typeUV u
                   | UConst c -> typeOf typeConst c
                   | UAdd (a, _) -> typeOfM a
                   | USub (a, _) -> typeOfM a
                   | UMul (a, _) -> typeOfM a
                   | UDiv (a, _) -> typeOfM a
                   | UMod (a, _) -> typeOfM a
                   | UNot _ -> Bool
                   | UAnd _ -> Bool
                   | UEq (_, _) -> Bool
                   | ULt (_, _) -> Bool
                   | UMux (_, a, _) -> typeOfM a
                   | UPow (a, _) -> typeOfM a
               end
                          
let typeUE : ue type_impl =
  (module Type_ue : TYPE with type t = ue)
    
let constant : type a. a typ -> a -> const =
  fun t x -> match t with
             | Int64 -> CInt64 x | Int32 -> CInt32 x | Int16 -> CInt16 x
             | Int8 -> CInt8 x | UInt64 -> CWord64 x | UInt32 -> CWord32 x
             | UInt16 -> CWord16 x | UInt8 -> CWord8 x
             | Bool -> CBool x | Int -> CInt x
             | Float -> CFloat x
                               
let ubool (a : bool) = UConst (CBool a)
let unot = function
  | UConst (CBool b) -> ubool (not b)
  | UNot b -> b
  | b -> UNot b
              
let uand a b =
  let reduceAnd ls = UAnd ls in
  if a == b then a else
    match a, b with
    | (UConst (CBool false), _ ) -> a
    | (_, UConst (CBool false)) -> b
    | (UConst (CBool true), _ ) -> b
    | (_, UConst (CBool true)) -> a
    | ((UAnd x), (UAnd y)) -> reduceAnd (x @ y)
    | ((UAnd x), _ ) -> reduceAnd (b :: x)
    | (_, (UAnd y)) -> reduceAnd (a :: y)
    | _, _ -> reduceAnd [a; b]
                        
let ueq a b =
  if a == b then (ubool true)
  else
    match a, b with
    | (UConst (CBool a), UConst (CBool b)) -> ubool (a == b)
    | (UConst (CInt64 a), UConst (CInt64 b)) -> ubool (a == b)
    | (UConst (CInt32 a), UConst (CInt32 b)) -> ubool (a == b)
    | (UConst (CInt16 a), UConst (CInt16 b)) -> ubool (a == b)
    | (UConst (CInt8 a), UConst (CInt8 b)) -> ubool (a == b)
    | (UConst (CWord64 a), UConst (CWord64 b)) -> ubool (a == b)
    | (UConst (CWord32 a), UConst (CWord32 b)) -> ubool (a == b)
    | (UConst (CWord16 a), UConst (CWord16 b)) -> ubool (a == b)
    | (UConst (CWord8 a), UConst (CWord8 b)) -> ubool (a == b)
    | (UConst (CInt a), UConst (CInt b)) -> ubool (a == b)
    | (UConst (CFloat a), UConst (CFloat b)) -> ubool (a == b)
    | _, _ -> UEq (a, b)
                  
let ult a b =
  if a == b then (ubool false)
  else
    match a, b with
    | (UConst (CBool a), UConst (CBool b)) -> ubool (a < b)
    | (UConst (CInt64 a), UConst (CInt64 b)) -> ubool (a < b)
    | (UConst (CInt32 a), UConst (CInt32 b)) -> ubool (a < b)
    | (UConst (CInt16 a), UConst (CInt16 b)) -> ubool (a < b)
    | (UConst (CInt8 a), UConst (CInt8 b)) -> ubool (a < b)
    | (UConst (CWord64 a), UConst (CWord64 b)) -> ubool (a < b)
    | (UConst (CWord32 a), UConst (CWord32 b)) -> ubool (a < b)
    | (UConst (CWord16 a), UConst (CWord16 b)) -> ubool (a < b)
    | (UConst (CWord8 a), UConst (CWord8 b)) -> ubool (a < b)
    | (UConst (CInt a), UConst (CInt b)) -> ubool (a < b)
    | (UConst (CFloat a), UConst (CFloat b)) -> ubool (a < b)
    | _, _ -> ULt (a, b)
                  
let rec umux b t f =
  match b, t, f with
  | ((UConst (CBool b1)), t, f) -> if b1 then t else f
  | ((UNot b1), t, f) -> umux b1 f t
  | (b1, (UMux (b2, t1, _)), f) -> if b1 == b2 then umux b1 t1 f
                                   else umux b t f
  | (b1, t, (UMux (b2, _, f1))) -> if b1 == b2 then umux b1 t f1
                                   else umux b t f
  | (_, _, _) -> UMux (b, t, f)
                      
let rec uecv : type a. a e -> ue = function
                                 | VRef (V (v, _)) -> UVRef v
                                 | Const (a, b) -> UConst (constant a b)
                                 | Add (a, b) -> UAdd ((uecv a), (uecv b))
                                 | Sub (a, b) -> USub ((uecv a), (uecv b))
                                 | Mul (a, b) -> UMul ((uecv a), (uecv b))
                                 | Div (a, b) -> UDiv ((uecv a), (uecv b))
                                 | Mod (a, b) -> UMod ((uecv a), (uecv b))
                                 | Not a -> unot (uecv a)
                                 | And (a, b) -> uand (uecv a) (uecv b)
                                 | Eq (a, b) -> ueq (uecv a) (uecv b)
                                 | Lt (a, b) -> ult (uecv a) (uecv b)
                                 | Mux (a, b, c) -> umux (uecv a) (uecv b) (uecv c)
                                 | Pow (a, b) -> UPow ((uecv a), (uecv b))
                                                      
let uvcv (V (v, _)) = v
                        
let cInt x = Const (Int, x)
let cInt64 x = Const (Int64, Int64.of_int x)
let cInt32 x = Const (Int32, Int32.of_int x)
let cInt16 x = Const (Int16, Int16.of_int x)
let cInt8 x = Const (Int8, Int8.of_int x)
let cWord64 x = Const (UInt64, Uint64.of_int x)
let cWord32 x = Const (UInt32, Uint32.of_int x)
let cWord16 x = Const (UInt16, Uint16.of_int x)
let cWord8 x = Const (UInt8, Uint8.of_int x)
let cBool x = Const (Bool, x)
let cFloat x = Const (Float, x)
                     
let and_ ls = List.fold_left (fun x y -> And (x, y)) (cBool true) ls
                             
let ueUpstream = function
  | UVRef (UV (_, _, _)) -> []
  | UVRef (UVArray (_, a)) -> [a]
  | UVRef (UVExtern (_, _)) -> []
  | UConst _ -> []
  | UAdd (a, b) -> [a; b]
  | USub (a, b) -> [a; b]
  | UMul (a, b) -> [a; b]
  | UDiv (a, b) -> [a; b]
  | UMod (a, b) -> [a; b]
  | UNot a -> [a]
  | UAnd a -> a
  | UEq (a, b) -> [a; b]
  | ULt (a, b) -> [a; b]
  | UMux (a, b, c) -> [a; b; c]
  | UPow (a, b) -> [a; b]

let uvUpstream u =
  let rec erase = function
    | [] -> []
    | r :: rs -> if List.mem r rs then erase rs
                 else r :: erase rs in
  let rec col = function
    | UVRef u -> [u]
    | u -> List.flatten (List.map col (ueUpstream u)) in
  erase (col u)
        
let not_ a = Not a
let (@!) : type a b. a tArray -> b e -> a v =
  fun arr ind -> match arr with
                 | A (uav, t) -> V (UVArray (uav, uecv ind), t)
                                 
let (@!.) : type a b. a tArray -> b e -> a e =
  fun arr ind -> VRef (arr @! ind)
                      
let (==.) a b = Eq (a, b)
let (/=.) a b = not_ (a ==. b)
let (<.) a b = Lt (a, b)
let (>.) a b = Lt (b, a)
let (<=.) a b = not_ (a >. b)
let (>=.) a b = not_ (a <. b)
let (@+) a b = Add (a, b)
let (@-) a b = Sub (a, b)
let (@/) a b = Div (a, b)
let (@++) a b = Mul (a, b)
let (@*+) a b = Pow (a, b)
let div_ a b = Div (a, b)
let mod_ a b = Mod (a, b)
let value a = VRef a
let mux a b c = Mux (a, b, c)
