type port = int
type host

val conn : host -> port -> (in_channel -> out_channel -> 'a) -> 'a
val conn_ip : string -> port -> (in_channel -> out_channel -> 'a) -> 'a
val conn_hostname : string -> port -> (in_channel -> out_channel -> 'a) -> 'a
