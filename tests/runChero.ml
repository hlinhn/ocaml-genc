open Entry
open DeclNode
open WriteCode
open TypeExpr
open Corenode
open Stdint

let precode = {|uint64_t time;|}
let postcode =
{|void loop() {
    blinky();
 }|}
                                  
let conf = { defaultConf with
             arduino = true;
             cCode = (precode, postcode);
             inout = [DOut 13];
           }
let blinky =
  do_
  ; old_time <-- var "old_time" UInt64 (Uint64.of_int 0)
  ; led <-- var "ledstate" Bool true
  ; ledval <-- var "ledvalue" UInt8 (Uint8.of_int 0)
  ; let time = var' "time" UInt64 in 
    let timer = 
      do_
      ; cond ((value time @- value old_time) >=. (cWord64 1000))
      ; old_time <== (value time)
      ; led <== not_ (value led) in
    let switch =
      do_
      ; ledval <== mux (value led) (cWord8 1) (cWord8 0) in
    let update s =
      let e = value s in
      do_
      ; action (fun v -> Printf.sprintf "%s = millis()" (List.hd v)) [uecv e]
      ; s <== e in
    node "change_time" (update time)
    ; node "update_time" timer
    ; node "change_state" switch
    ; writePin (DOut 13) (uvcv ledval)

let () = entry conf "blinky" blinky
