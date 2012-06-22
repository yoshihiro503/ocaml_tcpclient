open Util
module Soc = Socket

type request = {
    port : int;
    hostname : string;
    path : string;
    post_data : string;
    headers : (string * string) list;
    get_params : (string * string) list;
  }

let request port hostname path = {
  port = port;
  hostname = hostname;
  path = path;
  post_data = "";
  headers = [];
  get_params = [];
}

let set_post_data req data = { req with post_data = data}

let add_header req header value =
  { req with headers = (header,value)::req.headers }

let add_get_parameter req param value =
  { req with get_params = (param,value)::req.get_params }

let url_encode s =
  let ss = StringUtil.foldr (fun c store -> match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' | '~' as c ->
        String.make 1 c :: store
    | c -> (!%"%%%X" (int_of_char c)) :: store) s []
  in
  String.concat "" (ss)

let html_decode s =
  let rec aux store = function
    | '&'::'a'::'m'::'p'::';'::cs -> aux ('&'::store) cs
    | '&'::'q'::'u'::'o'::'t'::';'::cs -> aux ('\"'::store) cs
    | '&'::'l'::'t'::';'::cs -> aux ('<'::store) cs
    | '&'::'g'::'t'::';'::cs -> aux ('>'::store) cs
    | c :: cs -> aux (c::store) cs
    | [] -> List.rev store
  in
  StringUtil.of_list @@ aux [] @@ StringUtil.to_list s

let connect req f =
  let headers = slist "" (fun (k,v) -> !%"%s: %s\r\n" k v) req.headers in
  let path = req.path ^ "?"
    ^ slist "&" (fun (k,v) -> k^"="^url_encode v) req.get_params
  in
  let message =
    if req.post_data = "" then
      !%"GET %s HTTP/1.0\r\n" path
      ^ headers
      ^ "Host: " ^ req.hostname ^ "\r\n"
      ^ "\r\n"
    else
	!%"POST %s HTTP/1.0\r\n" path
	^ "Host: " ^ req.hostname ^ "\r\n"
	^ !%"Content-Length: %d\r\n" (String.length req.post_data)
	^ headers
	^ "\r\n"
	^ req.post_data
	^ "\r\n"
  in
  let read_header in_ =
    let rec iter store =
      match input_line in_ with
      | "" | "\r" -> List.rev store
      | line ->
          let (key,value) = begin match Str.split (Str.regexp ":") line with
          | [key] -> (key, "")
          | [key; value] -> (key, value)
          | key :: values -> (key, String.concat ":" values)
          | [] -> failwith ("read_header: "^line)
          end in
          iter ((key,value)::store)
    in
    iter []
  in
  Soc.conn_hostname req.port req.hostname begin fun in_ out ->
    output_string out message; flush out;
    f (read_header in_) in_
  end

type url = string

let get url =
  if String.sub url 0 7 = "http://" then
    String.sub url 7 (String.length url - 7)
    |> Str.split (Str.regexp "/")
    |> function
        | host :: path_list ->
            let path = "/" ^ String.concat "/" path_list in
            let req = request 80 host path in
            let read input =
              let rec iter store =
                match may input_line input with
                | Inl line -> iter (line :: store)
                | Inr _ -> List.rev store
              in
              iter []
            in
            connect req begin fun _ input ->
              read input
            end
        | [] -> raise (Invalid_argument url)
  else
     raise (Invalid_argument url)
