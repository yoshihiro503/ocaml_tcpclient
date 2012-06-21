open Util

type port = int
type host = Unix.inet_addr

let conn port host =
  let sockaddr : Unix.sockaddr = Unix.ADDR_INET (host, port) in
  fun f ->
    let (in_ch, out_ch) = Unix.open_connection sockaddr in
    let result = may (fun (i,o) -> f i o) (in_ch, out_ch) in
    may close_out out_ch |> ignore;
    may Unix.shutdown_connection in_ch |> ignore;
    match result with
    | Inl success_result -> success_result
    | Inr err -> raise err

let conn_ip port ipname =
  conn port @@ Unix.inet_addr_of_string ipname

let conn_hostname port hostname =
  conn port @@ (Unix.gethostbyname hostname).Unix.h_addr_list.(0)
