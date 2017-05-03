open Entry
open DeclNode
open WriteCode
open TypeExpr
open Corenode
open Stdint

(* here we create a new type, which is just a container
     of a variable of type unsigned int, 64 bits *)
type timer = Timer of (uint64 v) | X of (bool v)
                        
(* create_timer returns a node containing a value of timer type *)   
let create_timer name =
  do_
  ; t <-- var name UInt64 (Uint64.of_int 0)
  ; return (Timer t)
(* gclock is the global_clock defined by the scheduling component *)
let gclock = value (var' "__global_clock" UInt64)
let start_timer t time = match t with
  | Timer v -> v <== (gclock @+ time)
  | X b -> failwith "th"
let isTimerDone t =
  match t with
  | Timer t' -> (value t') <=. gclock
  | X b' -> (value b') /=. (cBool true)

(* the debouncer makes use of the timer type, demonstrating the
     way nodes can be composed to create more complicated structure *)
let debounce name ont offt sstate deb =
  do_
  ; last <-- var "last" Bool sstate
  ; out <-- var "out" Bool sstate
  ; t <-- create_timer "timer"
  ; let oncode =
      do_
      ; cond (and_ [deb; not_ (value last)])
      ; start_timer t ont
      ; last <== deb in
    node "on" oncode
    ; let offcode =
        do_
        ; cond (not_ (and_ [deb; (value last)]))
        ; start_timer t offt
        ; last <== deb in
      node "off" offcode
      ; let setcode =
          do_
          (* only set the result when the input is
       stable for a long enough period of time *)
          ; cond (deb ==. value last)
          ; cond (isTimerDone t)
          ; out <== value last in
        node "set" setcode
        ; return (value out)

let test =
  do_
  ; e <-- var "e" Bool true
  ; debounce "deb" (cWord64 5) (cWord64 5) false (value e)
             
let () = entry defaultConf "debouncer" test
