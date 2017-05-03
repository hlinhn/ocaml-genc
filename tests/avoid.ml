open Entry
open DeclNode
open WriteCode
open TypeExpr
open Corenode
open Stdint
       
let precode = {|
#include <Servo.h>
uint64_t time;
float distance;
Servo myservo;|}
                
let postcode =
{|
void readDist() {
    distance = pulseIn(9, LOW) / 50;
}
void loop() {
    runaround();
 }|}
                                
let conf = { defaultConf with
             arduino = true;
             cCode = (precode, postcode);
             inout = [DOut 4; DOut 7; AOut 5; AOut 6; AOut 11;
                     DOut 10; AIn 9];
             libs = ["myservo.attach(11)"];
           }
let turn =
  do_
  ; left <-- var "left" UInt8 (Uint8.of_int 1)
  ; right <-- var "right" UInt8 (Uint8.of_int 1)
  ; lval <-- var "lval" UInt8 (Uint8.of_int 250)
  ; rval <-- var "rval" UInt8 (Uint8.of_int 250)
  ; obs <-- var "obs" Bool false
  ; pos <-- var "pos" UInt8 (Uint8.of_int 120)
  ; dir <-- var "dir" Bool false
  ; let dist = var' "distance" Float in    
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
      let back_off c l r =
        do_
        ; cond (value c ==. cWord8 (int_of_char 'D'))
        ; left <== cWord8 0
        ; right <== cWord8 1
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
        ; node "out" (do_
                     ; cond (value dist >=. cFloat 20.)
                     ; command <== cWord8 (int_of_char 'U')
                     ; obs <== cBool false)
               
        ; node "close" (do_
                       ; cond (value dist <=. cFloat 15.)
                       ; command <== cWord8 (int_of_char 'D'))
        ; node "turn" (do_
                      ; cond (and_ [(value dist >. cFloat 15.); (value dist <. cFloat 20.)])
                      ; obs <== cBool true
                      ; node "r" (do_
                                 ; cond (value pos >=. cWord8 120)
                                 ; command <== cWord8 (int_of_char 'R')
                                 ; dir <== cBool true)
                      ; node "l" (do_
                                 ; cond (value pos <. cWord8 120)
                                 ; command <== cWord8 (int_of_char 'L')
                                 ; dir <== cBool false)) in
      let turn_head =
        do_
        ; cond (not_ (value obs))
        ; node "cdt" (do_
                    ; cond (value pos >. cWord8 180)
                    ; dir <== cBool true)
        ; node "cdf" (do_
                     ; cond (value pos <. cWord8 60)
                     ; dir <== cBool false)
        ; node "incr" (do_
                      ; cond (value dir)
                      ; pos <== value pos @- cWord8 1)
        ; node "decr" (do_
                      ; cond (not_ (value dir))
                      ; pos <== value pos @+ cWord8 1)
        ; action (fun v -> Printf.sprintf "myservo.write(%s)" (List.hd v)) [uecv (value pos)] in 
      
      let get_dist =
        do_
        ; v1 <-- var "v1" UInt8 (Uint8.of_int 0)
        ; v2 <-- var "v2" UInt8 (Uint8.of_int 1)
        ; writePin (DOut 10) (uvcv v1)
        ; writePin (DOut 10) (uvcv v2)
        ; action (fun _ -> "readDist()") [] in 
      node "update" update
      ; node "get_dist" get_dist
      ; node "turn_head" turn_head
      ; node "turn_left" (left_turn command 250 250)
      ; node "turn_right" (right_turn command 250 250)
      ; node "straight" (straight command 190 190)
      ; node "back_off" (back_off command 130 130)
      ; node "stop" (stop command)
      ; writePin (DOut 4) (uvcv left)
      ; writePin (DOut 7) (uvcv right)
      ; writePin (AOut 5) (uvcv lval)
      ; writePin (AOut 6) (uvcv rval)

let () = entry conf "runaround" turn
