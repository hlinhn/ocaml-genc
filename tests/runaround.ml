open Entry
open DeclNode
open WriteCode
open TypeExpr
open Corenode
open Stdint
       
let precode = {|uint64_t time;
bool avail;|}
let postcode =
{|void loop() {
    avail = Serial.available();
    runaround();
 }|}
                                
let conf = { defaultConf with
             arduino = true;
             cCode = (precode, postcode);
             inout = [DOut 4; DOut 7; AOut 5; AOut 6];
             libs = ["Serial.begin(115200)"];
           }
let turn =
  do_
  ; left <-- var "left" UInt8 (Uint8.of_int 1)
  ; right <-- var "right" UInt8 (Uint8.of_int 1)
  ; lval <-- var "lval" UInt8 (Uint8.of_int 250)
  ; rval <-- var "rval" UInt8 (Uint8.of_int 250)
  ; let avail = var' "avail" Bool in
    let conv x = Uint8.of_int (int_of_char x) in
    command <-- var "command" UInt8 (conv 'U')
    ; let left_turn c l r =
        do_
        ; cond (value c ==. cWord8 (int_of_char 'L'))
        ; left <== cWord8 1
        ; right <== cWord8 1
        ; lval <== cWord8 l
        ; rval <== cWord8 r in
      let right_turn c l r =
        do_
        ; cond (value c ==. cWord8 (int_of_char 'R'))
        ; left <== cWord8 0
        ; right <== cWord8 0
        ; lval <== cWord8 l
        ; rval <== cWord8 r in
      let straight c l r =
        do_
        ; cond (value c ==. cWord8 (int_of_char 'U'))
        ; left <== cWord8 1
        ; right <== cWord8 0
        ; lval <== cWord8 l
        ; rval <== cWord8 r in
      let stop c =
        do_
        ; cond (value c ==. cWord8 (int_of_char 'S'))
        ; left <== cWord8 0
        ; right <== cWord8 0
        ; lval <== cWord8 0
        ; rval <== cWord8 0 in
      let update =
        do_
        ; let e = value command in
          action (fun v -> Printf.sprintf "%s = Serial.read()" (List.hd v)) [uecv e]
          ; command <== e in
      node "update" (do_
                    ; cond (value avail)
                    ; update)
      ; node "turn_left" (left_turn command 250 250)
      ; node "turn_right" (right_turn command 250 250)
      ; node "straight" (straight command 190 190)
      ; node "stop" (stop command)
      ; writePin (DOut 4) (uvcv left)
      ; writePin (DOut 7) (uvcv right)
      ; writePin (AOut 5) (uvcv lval)
      ; writePin (AOut 6) (uvcv rval)

let () = entry conf "runaround" turn
