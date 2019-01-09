open Printf
open Helpers
module Http_client = Nethttp_client
open Http_client.Convenience


let http_get  = Http_client.Convenience.http_get
let http_post = Http_client.Convenience.http_post
let to_json = Yojson.Safe.from_string
let print_json = Yojson.Safe.pretty_to_channel stdout

module H = BatHashtbl 
module L = BatList
module A = BatArray
module Y = Yojson
module YB = Yojson.Basic
module YU =  struct
  let drop_assoc = function `Assoc xs -> xs | _ -> failwith "Bad argument"
  let drop_string = function `String s -> s | _ -> failwith "Bad argument"
  let drop_int = function `Int n -> n | _ -> failwith "Bad argument"
  let drop_list = function `List xs -> xs | _ -> failwith "Bad argument"
  let unwrap_res x = x |> drop_assoc |> List.assoc "data"
end



let match_regexp regexpr chaine =
  let rex = Netstring_pcre.regexp regexpr in
  let results = Netstring_pcre.full_split rex chaine in
    match results with
      | [Netstring_pcre.Text s]       -> []
      | (Netstring_pcre.Delim s)::q   -> List.map (fun el -> match el with
                                                 | Netstring_pcre.Group (n,s) -> s
                                                 | _ -> "") q
      | _                            -> [];;


let talkToNeo ?(verbose=false) server port login passwd postData = (*On créé une transaction*)
                let url = sprintf "http://%s:%d/db/data/transaction/" server port in
                let passneo =  sprintf "neo4j:%s" passwd |> BatBase64.str_encode in
                if verbose then begin print_endline url; print_endline passneo;  end;
                let req = new Http_client.post_raw url postData in
                if verbose then print_endline "connexion ok...";
                let pipeline = new Http_client.pipeline in
                if verbose then print_endline "pipe...";
                if verbose then req#set_req_header "Authorization" passneo;
                if verbose then req#set_req_header "Content-type" "application/json";
                if verbose then print_endline "headers..";
                pipeline#add_with_callback req @@
                (fun call -> match call#response_status with
                | `Ok -> ()
                | `Bad_request -> 
                                print_endline call#response_body#value;
                    let j = to_json call#response_body#value in
                    j |> YU.drop_assoc |> List.assoc "message"
                      |> YU.drop_string |> print_endline;
                 | _ -> 
                                 print_endline call#response_status_text;
                                 print_endline call#response_body#value;
                                 (*print_endline "callback";*)
                                 ()
                 );
                 pipeline#run ();
                 let (ans: string) = req#get_resp_body () in
                 (*let urls = match_regexp ".+?commit\\\":\\\"http:.+?db.data.transaction.(\d+)\/commit\\\".*" ans in
                 let res = if BatList.length urls > 0 then BatList.hd urls else "" in*)
                 if verbose then print_endline ans;
                 ans;;



class neoResult = object(self)
        val mutable columnList = []
        val mutable hColumns   = H.create 345
        val mutable linesMeta  = [||]
        val mutable lines      = [||]

        method parseResult res = let json  = res |> YB.from_string in
                              let start = L.at (json |> YU.drop_assoc) 1 |> snd |> YU.drop_list |> L.hd |> YU.drop_assoc in
                              let cols, mylines = L.hd start, L.at start 1 in
                              columnList <- L.map (fun n ->  YU.drop_string n) (snd cols |> YU.drop_list);
                              linesMeta <- snd mylines |> YU.drop_list |> L.map (fun el -> L.at (YU.drop_assoc el) 1 |> snd ) |> A.of_list;
                              lines     <- snd mylines |> YU.drop_list |> L.map (fun el -> YU.drop_assoc el |> L.hd  |> snd ) |> A.of_list;
                              L.iteri (fun idx -> fun col -> A.iter (fun ligne -> H.add hColumns col (L.at (ligne|> YU.drop_list) idx)) lines )
                              columnList;
                              ()

        method getAll = columnList, hColumns, linesMeta, lines

        method getColumn col = H.find_all hColumns col
        method getLine   n   = lines.(n)
        method getLineMeta n = linesMeta.(n)
        method getColumnForLine col n = let l = H.find_all hColumns col in L.at l n
end


class neo4jConnector server port login passwd  = object(self)
        val mutable idTransaction = 0
        method initTransaction  = let result = talkToNeo ~verbose:true server port login passwd "" in
                                  let urls = match_regexp ".+?commit\":\"http:.+?db.data.transaction.(\\d+)\\/commit\".*" result in
                                  let res = if BatList.length urls > 0 then BatList.hd urls else "" in
                                  idTransaction <- (int_of_string res);
                                  int_of_string res

        method endTransaction = true
        method cypher req params = (*let buildParams params : (string * YojsonBasic.json) = "parameters",  `Assoc (L.map (fun (a,b) -> (a,`String b)) params) in*)
                                   let args = `Assoc [("statements", `List [`Assoc [("statement", `String req); ("parameters", `Assoc params) ]])] in
                                   let req  = YB.to_string args in
                                   let result = talkToNeo ~verbose:true server port login passwd req in
                                   print_endline result;
                                   result;

end


(*

module type CONFIG = sig val server:string val port:int end

module Make(Cfg: CONFIG) = struct

type cypher_msg = string
let string_of_cypher_msg (x:cypher_msg) = x

type transaction = int


let create_transaction ?(verbose=false) passwd =
        let url = sprintf "http://%s:%d/db/data/transaction/" Cfg.server Cfg.port in
        let passneo =  sprintf "neo4j:%s" passwd |> BatBase64.str_encode in
        if verbose then begin print_endline url; print_endline passneo;  end;
        let req = new Http_client.post_raw url "" in
        if verbose then print_endline "connexion ok...";
        let pipeline = new Http_client.pipeline in
        if verbose then print_endline "pipe...";
        if verbose then req#set_req_header "Authorization" passneo;
        if verbose then req#set_req_header "Content-type" "application/json";
        if verbose then print_endline "headers..";
        pipeline#add_with_callback req @@
         (fun call -> match call#response_status with
         | `Ok -> ()
         | `Bad_request -> 
                    print_endline call#response_body#value;
                    let j = to_json call#response_body#value in
                    j |> YoUtil.drop_assoc |> List.assoc "message"
                      |> YoUtil.drop_string |> print_endline;
                 | _ -> 
                    print_endline call#response_status_text;
                    print_endline call#response_body#value;
                    (*print_endline "callback";*)
                    ()
    );
  pipeline#run ();
  let (ans: string) = req#get_resp_body () in
  let urls = match_regexp ".+?commit\\\":\\\"http:.+?db.data.transaction.(\d+)\/commit\\\".*" ans in
  let res = if BatList.length urls > 0 then BatList.hd urls else "" in
  if verbose then print_endline ans;
  res;;

let post_cypher ?(params=[]) url passwd cypher =
  let passneo =  sprintf "neo4j:%s" passwd |>19dd BatBase64.str_encode in
  let pipeline = new Http_client.pipeline in
  let opt = pipeline # get_options in
  pipeline # set_options
    { opt with
        Http_client.number_of_parallel_connections = 1;
        Http_client.verbose_status = true;
        Http_client.verbose_connection = true;
        Http_client.verbose_response_header = true;
        Http_client.verbose_response_contents = true;
        Http_client.verbose_request_header = true;
        Http_client.verbose_request_contents = true;  };
  let args = `Assoc [("statements", `List [`Assoc [("statement", `String cypher)]])] in
  print_endline @@ Str.global_replace (Str.regexp "\n") " " cypher;
  let req = new Http_client.post_raw url (Yojson.to_string args) in
   req#set_req_header "Accept"       "application/json; charset=UTF8";
   req#set_req_header "Content-type" "application/json";
   pipeline#add_with_callback req @@
    (fun call -> match call#response_status with
                 | `Ok -> ()
                 | `Bad_request ->
                    let j = to_json call#response_body#value in
                    j |> YoUtil.drop_assoc |> List.assoc "message"
                      |> YoUtil.drop_string |> print_endline;
                 | _ ->
                    print_endline call#response_status_text;
                    print_endline call#response_body#value;
                    print_endline "callback"

    );
  pipeline#run ();
  req#get_resp_body ();;

*)
(*
let wrap_cypher ?(verbose=true) cmd ~params ~f =
  let (ans: string) = post_cypher ~params cmd in
  if verbose then print_endline ans;
  ans |> to_json |> YoUtil.drop_assoc |> List.assoc "data" |> f


let remove_all ?(verbose=false) () : (_,_) Result.t =
  wrap_cypher ~verbose ~params:[] ~f:(fun _ -> () )
     "START r=rel(*)  DELETE r;";
  wrap_cypher ~verbose ~params:[] ~f:(fun _ -> () )
     "START n=node(*) DELETE n;";
  OK ()

let insert_node_between id1  =
  let cmd = sprintf
	"START n=node(%d)
         MATCH n-[r:FOLLOWED_BY]->m
         DELETE r
	 CREATE UNIQUE n-[r1:FOLLOWED_BY]->(k{title:'qwe'})-[r2:FOLLOWED_BY]->m
	 set k: TIMELINE_ITEM
	" id1
  in
  print_endline (Str.global_replace (Str.regexp "\n") cmd " ");
  post_cypher cmd




class node_of_json (j: (string * Yojson.json) list) = object
  method id : int =
    match List.assoc "self" j with
    | `String s -> int_of_string @@ String.rsplit s ~by:'/'
    | _ -> failwith "Wrong json"
  method json = j
  method data = List.assoc "data" j
  method prop name = List.assoc name j
end

class date_of_json (j: (string * Yojson.json) list) = object
  method id : int =
    match List.assoc "self" j with
    | `String s -> int_of_string @@ String.rsplit s ~by:'/'
    | _ -> failwith "Wrong json"
  method json = j
  method data = List.assoc "data" j
  method when_ = match List.assoc "data" j with
    | `Assoc xs -> List.assoc "when" xs |> (function `String s -> s | _ -> assert false)
    | _ -> assert false
end

class question_of_json (j: (string * Yojson.json) list) = object(self)
  method id : int =
    match List.assoc "self" j with
    | `String s -> int_of_string @@ String.rsplit s ~by:'/'
    | _ -> failwith "Wrong json"
  method json = j
  method data = List.assoc "data" j
  method prop name = match List.assoc "data" j with
    | `Assoc xs -> List.assoc name xs |> (function `String s -> s | _ -> assert false)
    | _ -> assert false
  method text = self#prop "text"
end


let get_questions nodeid =
  let cmd = sprintf "START x=node(%d)
                     MATCH x-[:HAS_QUESTION]->y RETURN y" nodeid in
  let j = to_json @@ post_cypher cmd |> YoUtil.drop_assoc |> List.assoc "data" in
  print_endline @@ Yojson.to_string j;
  match j with
  | `List[`List ys ] -> OK (List.map (fun y -> new question_of_json (YoUtil.drop_assoc y)) ys)
  | _ -> Error "JSON match failure"

let get_next_timeline_node nodeid =
  let cmd = sprintf "START x=node(%d)
                     MATCH x-[:FOLLOWED_BY]->y RETURN y" nodeid in
  match to_json @@ post_cypher cmd |> YoUtil.drop_assoc |> List.assoc "data" with
  | `List[`List[`Assoc xs]] -> OK (new node_of_json xs)
  | _ -> Error "JSON match failure"

let id_from_node_json ej =
  match List.Assoc.find_exn ej "self" with
  | `String s -> Int64.of_string @@ String.rsplit s ~by:'/'
  | _ -> failwith "Wrong json for function id_from_node_json"
*)






