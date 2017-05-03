open Entry
open DeclNode
open WriteCode
open TypeExpr
open Corenode
open Stdint
       
let precode = {|#include <std_msgs/String.h>
std_msgs::String str_msg;
char hello[13] = "hello world!";|}

let ros = { defaultHandler with id = "nh";
                                pubs = [{pubid = "chatter";
                                         topic = "chatter";
                                         msg = "str_msg"}] }
let postcode = String.concat "\n"
                              [{|void loop() {
    str_msg.data = hello;|}; (publish ros);
                               {|   hello1();
}|}]
                                
let conf = { defaultConf with
             arduino = true;
             rosserial = true;
             cCode = (precode, postcode);
             roshandles = ros
           }
let hello1 =
  do_
  ; let spin =
      do_
      ; action (fun _ -> Printf.sprintf "%s.spinOnce()" ros.id) []
      ; action (fun _ -> "delay(1000)") [] in    
    node "main" spin
         
let () = entry conf "hello1" hello1
