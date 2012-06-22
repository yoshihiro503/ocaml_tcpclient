type request
val request : int -> string -> string -> request
val connect : request -> ((string * string)list -> in_channel -> 'a) -> 'a
val set_post_data : request -> string -> request

(** [add_header req header value] *)
val add_header : request -> string -> string -> request

(** [add_get_parameter req param value] *)
val add_get_parameter : request -> string -> string -> request

type url = string

val get : url -> string list
