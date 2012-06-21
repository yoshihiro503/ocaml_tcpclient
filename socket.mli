(**
   A simple TCP Socket client
*)

(** port number *)
type port = int

(**
   connection with ipaddress as string
   {[
   conn_ip 80 "59.106.178.240" begin fun in_ out ->
     ...
   end
   ]}
 *)
val conn_ip : port -> string -> (in_channel -> out_channel -> 'a) -> 'a

(**
   connection with hostname
   {[
   conn_hostname 80 "proofcafe.org" begin fun in_ out ->
     ...
   end
   ]}
 *)
val conn_hostname : port -> string -> (in_channel -> out_channel -> 'a) -> 'a
